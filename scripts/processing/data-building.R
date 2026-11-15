# =============================================================================
# UFPE - Departamento de Ciência Política
# ARQUIVO: votacao_secao_id.R
# OBJETIVO: série 1998-2022 da votação para governador de PE por seção
#           eleitoral, com identificador único da seção (ID_SECAO) e dados
#           dos candidatos, tudo via electionsBR.
#           1994 fica de fora: o TSE não publica a votação de PE por seção
#           nesse ano (a menor unidade disponível é a zona).
# SAÍDAS: dados/votacao_secao_PE_1998_2022_governador.{csv,rds}
# CODIFICAÇÃO: UTF-8 | R >= 4.1
# =============================================================================


if (!require(pacman)) {
  install.packages("pacman")
}

pacman::p_load(tidyverse, 
               geobr, 
               sf, 
               patchwork)

rm(list = ls())
cat("\014")

# GUIA RÁPIDO DE SINTAXE
# pacote::funcao()  função de um pacote específico
# <-                atribuição
# |>                pipe nativo: x |> f() equivale a f(x)
# %in%              "está contido em"
# base::lapply()    aplica uma função a cada elemento (aqui, a cada ano)
# dplyr::across()   aplica a mesma função a várias colunas


# =============================================================================
# PASSO 0: CONFIGURAÇÃO E PACOTES
# =============================================================================
ANOS  <- c(1998, 2002, 2006, 2010, 2014, 2018, 2022)
UF    <- "PE"
CARGO <- 3      # governador

PASTA <- "dados"
SAIDA <- base::file.path(PASTA, "votacao_secao_PE_1998_2022_governador")

COLUNAS_SECAO <- c(
  "ANO_ELEICAO", "NR_TURNO", "SG_UF", "CD_MUNICIPIO", "NM_MUNICIPIO",
  "NR_ZONA", "NR_SECAO", "CD_CARGO", "NR_VOTAVEL", "NM_VOTAVEL", "QT_VOTOS"
)

COLUNAS_CAND <- c(
  "ANO_ELEICAO", "SG_UF", "CD_CARGO", "NR_CANDIDATO", "SQ_CANDIDATO",
  "NM_CANDIDATO", "NM_URNA_CANDIDATO", "SG_PARTIDO"
)

base::options(timeout = 3600)   # downloads grandes
base::dir.create(PASTA, showWarnings = FALSE)

if (!base::requireNamespace("pacman", quietly = TRUE)) {
  utils::install.packages("pacman")
}

pacman::p_load(
  electionsBR,
  dplyr,
  readr,
  stringr,
  tidyselect
)


# Seleciona as colunas pedidas (ou para, listando as disponíveis) e passa
# tudo para texto, para que anos com tipos diferentes possam ser empilhados.
padronizar <- function(tabela, colunas, rotulo) {
  ausentes <- base::setdiff(colunas, base::colnames(tabela))
  if (base::length(ausentes) > 0) {
    base::stop(
      rotulo, ": faltam ", base::paste(ausentes, collapse = ", "),
      "\nDisponíveis: ", base::paste(base::colnames(tabela), collapse = ", ")
    )
  }
  tabela |>
    dplyr::select(tidyselect::all_of(colunas)) |>
    dplyr::mutate(dplyr::across(tidyselect::everything(), base::as.character))
}

# Baixa um tipo de arquivo do TSE para um ano e já padroniza.
baixar_tse <- function(ano, tipo, colunas) {
  base::message("   ", tipo, " ", ano)
  electionsBR::elections_tse(
    year = ano,
    type = tipo,
    uf   = UF
  ) |>
    padronizar(colunas, base::paste(tipo, ano))
}

# Maiúsculas e sem acento, para comparar nomes entre arquivos ("ZÉ" = "ZE").
sem_acento <- function(x) {
  stringi::stri_trans_general(base::toupper(x), "Latin-ASCII")
}


# =============================================================================
# PASSO 1: VOTAÇÃO POR SEÇÃO + ID_SECAO
# =============================================================================
# Baixa todos os anos, filtra governador e cria ID_SECAO.
# O número da seção se repete entre zonas e municípios; só a combinação
# UF + município + zona + seção é única. Ex.: "PE_25313_0001_0010".
# stringr::str_pad() completa com zeros para o código ter tamanho fixo.
# -----------------------------------------------------------------------------
base::message("PASSO 1: votação por seção")

votacao <- ANOS |>
  base::lapply(baixar_tse, tipo = "vote_section", colunas = COLUNAS_SECAO) |>
  dplyr::bind_rows() |>
  dplyr::mutate(
    dplyr::across(
      c(ANO_ELEICAO, NR_TURNO, CD_MUNICIPIO, NR_ZONA,
        NR_SECAO, CD_CARGO, NR_VOTAVEL, QT_VOTOS),
      base::as.numeric
    )
  ) |>
  dplyr::filter(CD_CARGO == CARGO) |>
  dplyr::mutate(
    ID_SECAO = base::paste(
      SG_UF,
      stringr::str_pad(CD_MUNICIPIO, 5, pad = "0"),
      stringr::str_pad(NR_ZONA,      4, pad = "0"),
      stringr::str_pad(NR_SECAO,     4, pad = "0"),
      sep = "_"
    )
  )

dplyr::glimpse(votacao)

votacao |>
  dplyr::distinct(ANO_ELEICAO, NR_TURNO, ID_SECAO) |>
  dplyr::count(ANO_ELEICAO, NR_TURNO, name = "n_secoes")

# Conferências: todos os anos presentes e ID único dentro de cada eleição
base::stopifnot(base::all(ANOS %in% votacao$ANO_ELEICAO))

duplicados <- votacao |>
  dplyr::count(ANO_ELEICAO, NR_TURNO, ID_SECAO, NR_VOTAVEL) |>
  dplyr::filter(n > 1)

if (base::nrow(duplicados) > 0) {
  base::stop(
    "ID_SECAO repetido, ex.: ",
    base::paste(utils::head(duplicados$ID_SECAO), collapse = ", ")
  )
}


# =============================================================================
# PASSO 2: CANDIDATOS
# =============================================================================
# O arquivo de seção traz só o número do candidato (NR_VOTAVEL) e o nome
# completo (NM_VOTAVEL); nome de urna, partido e SQ vêm do cadastro.
# Alguns números aparecem duas vezes no cadastro (candidato substituído que
# herda o número, ex.: 28 em 2010 e 50 em 2014). Fica o candidato cujo nome
# completo (NM_CANDIDATO) bate com o nome que recebeu os votos (NM_VOTAVEL).
# -----------------------------------------------------------------------------
base::message("PASSO 2: candidatos")

nomes_na_urna <- votacao |>
  dplyr::distinct(ANO_ELEICAO, NR_VOTAVEL, NM_VOTAVEL)

candidatos <- ANOS |>
  base::lapply(baixar_tse, tipo = "candidate", colunas = COLUNAS_CAND) |>
  dplyr::bind_rows() |>
  dplyr::mutate(
    dplyr::across(c(ANO_ELEICAO, CD_CARGO, NR_CANDIDATO), base::as.numeric)
  ) |>
  dplyr::filter(SG_UF == UF, CD_CARGO == CARGO) |>
  dplyr::distinct(
    ANO_ELEICAO, NR_CANDIDATO, SQ_CANDIDATO,
    NM_CANDIDATO, NM_URNA_CANDIDATO, SG_PARTIDO
  ) |>
  dplyr::left_join(
    nomes_na_urna,
    by = c("ANO_ELEICAO", "NR_CANDIDATO" = "NR_VOTAVEL")
  ) |>
  dplyr::add_count(ANO_ELEICAO, NR_CANDIDATO) |>
  dplyr::filter(
    n == 1 | sem_acento(NM_CANDIDATO) == sem_acento(NM_VOTAVEL)
  ) |>
  dplyr::select(-n, -NM_VOTAVEL)

dplyr::glimpse(candidatos)

# Cada ano + número deve ter sobrado exatamente uma vez
ainda_duplicados <- candidatos |>
  dplyr::count(ANO_ELEICAO, NR_CANDIDATO) |>
  dplyr::filter(n > 1)

if (base::nrow(ainda_duplicados) > 0) {
  base::stop(
    "Duplicatas sem resolução: ",
    base::paste(ainda_duplicados$ANO_ELEICAO, ainda_duplicados$NR_CANDIDATO,
                sep = "/", collapse = "; ")
  )
}


# =============================================================================
# PASSO 3: JUNÇÃO
# =============================================================================
# Junta por ANO + número (o mesmo número é de outro candidato em outro ano).
# relationship = "many-to-one" faz o left_join() parar se ainda houver
# duplicata no cadastro. Brancos (95) e nulos (96) ficam com NA nas
# colunas do candidato, como esperado.
# -----------------------------------------------------------------------------
base::message("PASSO 3: junção")

dados_secao <- votacao |>
  dplyr::left_join(
    candidatos,
    by           = c("ANO_ELEICAO", "NR_VOTAVEL" = "NR_CANDIDATO"),
    relationship = "many-to-one"
  ) |>
  dplyr::relocate(ID_SECAO, .after = CD_CARGO)

dplyr::glimpse(dados_secao)

base::stopifnot(base::nrow(dados_secao) == base::nrow(votacao))

# Votáveis sem cadastro: os votos continuam na base, só sem partido e SQ.
# É um aviso (não erro) porque o cadastro do TSE tem lacunas em anos
# antigos (ex.: Rivaldo Soares, 2006). Preencha o partido à mão se precisar.
sem_candidato <- dados_secao |>
  dplyr::filter(base::is.na(SQ_CANDIDATO), !NR_VOTAVEL %in% c(95, 96)) |>
  dplyr::distinct(ANO_ELEICAO, NR_VOTAVEL, NM_VOTAVEL)

sem_candidato

if (base::nrow(sem_candidato) > 0) {
  base::warning(
    "Sem candidato no cadastro: ",
    base::paste(sem_candidato$ANO_ELEICAO, sem_candidato$NM_VOTAVEL,
                collapse = "; ")
  )
}

# Total de votos por eleição: compare com o resultado oficial do TSE
# (atenção especial a 1998, cujos dados por seção podem estar incompletos)
dados_secao |>
  dplyr::summarise(
    votos = base::sum(QT_VOTOS),
    .by   = c(ANO_ELEICAO, NR_TURNO)
  )


# =============================================================================
# PASSO 4: LOCAL DE VOTAÇÃO, ENDEREÇO E COORDENADAS
# =============================================================================
# Acrescenta a cada seção o local de votação, endereço, CEP, latitude,
# longitude, situação e eleitorado apto (arquivo ELEITORADO_LOCAL_VOTACAO).
# - As coordenadas são do LOCAL: seções da mesma escola têm o mesmo ponto.
# - QT_ELEITOR_ELEICAO_ESTADUAL é o eleitorado apto para governador, já
#   descontadas as agregações (zero nas seções agregadas).
# - O TSE grava a coordenada do cadastro no momento em que gera o arquivo;
#   em anos antigos ela pode faltar ou não ser a da época.
# Anos sem arquivo de locais são avisados e ficam com NA nessas colunas.
# -----------------------------------------------------------------------------
base::message("PASSO 4: locais de votação e coordenadas")

# Arquivo de locais de votação (um .zip nacional por ano; %s = ano).
# Se der 404, confira o link em dadosabertos.tse.jus.br > Eleitorado - <ano>.
URL_LOCAIS <- "https://cdn.tse.jus.br/estatistica/sead/odsele/eleitorado_locais_votacao/eleitorado_local_votacao_%s.zip"

COLUNAS_LOCAL <- c(
  "AA_ELEICAO", "NR_TURNO", "SG_UF", "CD_MUNICIPIO", "NR_ZONA", "NR_SECAO",
  "CD_TIPO_SECAO_AGREGADA", "NR_SECAO_PRINCIPAL",
  "NR_LOCAL_VOTACAO", "NM_LOCAL_VOTACAO", "DS_ENDERECO", "NM_BAIRRO",
  "NR_CEP", "NR_LATITUDE", "NR_LONGITUDE", "CD_SITU_SECAO",
  "QT_ELEITOR_ELEICAO_ESTADUAL"
)

# Monta o mesmo ID_SECAO em qualquer tabela que tenha as quatro chaves.
criar_id_secao <- function(uf, municipio, zona, secao) {
  base::paste(
    uf,
    stringr::str_pad(municipio, 5, pad = "0"),
    stringr::str_pad(zona,      4, pad = "0"),
    stringr::str_pad(secao,     4, pad = "0"),
    sep = "_"
  )
}

# Baixa (com barra de progresso) e lê o arquivo de locais de votação de um
# ano, já filtrado para a UF. Devolve NULL, com aviso, se o ano não existir.
# Pelo leiame do TSE, #NULO/-1 = em branco e #NE/-3 = não registrado no ano:
# todos viram NA na leitura.
baixar_locais <- function(ano) {
  base::message("   locais de votação ", ano)
  zip <- base::file.path(PASTA, base::sprintf("eleitorado_local_votacao_%s.zip", ano))
  
  if (!base::file.exists(zip)) {
    baixou <- base::tryCatch(
      utils::download.file(
        url      = base::sprintf(URL_LOCAIS, ano),
        destfile = zip,
        mode     = "wb",
        method   = "libcurl",
        quiet    = FALSE
      ),
      error = function(e) {
        base::unlink(zip)
        NULL
      }
    )
    if (base::is.null(baixou)) {
      base::warning("Locais de votação indisponíveis para ", ano)
      return(NULL)
    }
  }
  
  csv <- utils::unzip(zip, exdir = PASTA)
  csv <- csv[base::grepl("\\.csv$", csv, ignore.case = TRUE)]
  
  readr::read_delim(
    csv,
    delim     = ";",
    locale    = readr::locale(encoding = "latin1"),
    na        = c("", "#NULO", "#NE", "-1", "-3"),
    col_types = readr::cols(.default = "c")
  ) |>
    dplyr::filter(SG_UF == UF) |>
    padronizar(COLUNAS_LOCAL, base::paste("locais", ano))
}


locais <- ANOS |>
  base::lapply(baixar_locais) |>
  dplyr::bind_rows() |>
  dplyr::rename(ANO_ELEICAO = AA_ELEICAO) |>
  dplyr::mutate(
    dplyr::across(
      c(ANO_ELEICAO, NR_TURNO, CD_MUNICIPIO, NR_ZONA, NR_SECAO,
        CD_TIPO_SECAO_AGREGADA, NR_SECAO_PRINCIPAL, NR_LOCAL_VOTACAO,
        CD_SITU_SECAO, QT_ELEITOR_ELEICAO_ESTADUAL),
      base::as.numeric
    ),
    dplyr::across(
      c(NR_LATITUDE, NR_LONGITUDE),
      \(x) base::as.numeric(stringr::str_replace(x, ",", "."))
    ),
    ID_SECAO = criar_id_secao(SG_UF, CD_MUNICIPIO, NR_ZONA, NR_SECAO)
  ) |>
  dplyr::select(-SG_UF, -CD_MUNICIPIO, -NR_ZONA, -NR_SECAO)

dplyr::glimpse(locais)
locais |> dplyr::count(ANO_ELEICAO, NR_TURNO, name = "n_secoes")

# Cada seção deve aparecer uma vez por ano e turno
locais_repetidos <- locais |>
  dplyr::count(ANO_ELEICAO, NR_TURNO, ID_SECAO) |>
  dplyr::filter(n > 1)

if (base::nrow(locais_repetidos) > 0) {
  base::stop(
    "Seção repetida no arquivo de locais, ex.: ",
    base::paste(utils::head(locais_repetidos$ID_SECAO), collapse = ", ")
  )
}

dados_secao <- dados_secao |>
  dplyr::left_join(
    locais,
    by           = c("ANO_ELEICAO", "NR_TURNO", "ID_SECAO"),
    relationship = "many-to-one"
  )

dplyr::glimpse(dados_secao)

base::stopifnot(base::nrow(dados_secao) == base::nrow(votacao))

# Cobertura por ano e turno: % de seções com local e com coordenada.
# Valores baixos indicam ano sem arquivo, turno ausente ou cadastro incompleto.
dados_secao |>
  dplyr::distinct(ANO_ELEICAO, NR_TURNO, ID_SECAO,
                  NR_LOCAL_VOTACAO, NR_LATITUDE) |>
  dplyr::summarise(
    n_secoes          = dplyr::n(),
    pct_com_local     = base::round(100 * base::mean(!base::is.na(NR_LOCAL_VOTACAO)), 1),
    pct_com_coordenada = base::round(100 * base::mean(!base::is.na(NR_LATITUDE)), 1),
    .by = c(ANO_ELEICAO, NR_TURNO)
  )

# Coordenadas devem cair dentro de PE (aprox. lat -10 a -3, lon -42 a -32,
# incluindo Fernando de Noronha)
fora_de_pe <- dados_secao |>
  dplyr::distinct(ANO_ELEICAO, ID_SECAO, NR_LATITUDE, NR_LONGITUDE) |>
  dplyr::filter(
    !base::is.na(NR_LATITUDE),
    NR_LATITUDE  < -10 | NR_LATITUDE  > -3 |
      NR_LONGITUDE < -42 | NR_LONGITUDE > -32
  )

if (base::nrow(fora_de_pe) > 0) {
  base::warning(base::nrow(fora_de_pe), " seções com coordenada fora de PE")
}


# =============================================================================
# PASSO 4B: USANDO A LONGITUDE E LATITUDE DE 2010 PARA 1998-2006
# =============================================================================

# Local e coordenadas de cada seção em 2010 (1º turno)
referencia_2010 <- dados_secao |>
  dplyr::filter(ANO_ELEICAO == 2010, NR_TURNO == 1) |>
  dplyr::distinct(
    ID_SECAO, NR_LOCAL_VOTACAO, NM_LOCAL_VOTACAO, DS_ENDERECO,
    NM_BAIRRO, NR_CEP, NR_LATITUDE, NR_LONGITUDE
  )

dplyr::glimpse(referencia_2010)

# Preenche 1998-2006 com os dados da mesma seção em 2010
dados_secao <- dados_secao |>
  dplyr::rows_patch(
    dados_secao |>
      dplyr::filter(ANO_ELEICAO < 2010) |>
      dplyr::distinct(ANO_ELEICAO, NR_TURNO, ID_SECAO) |>
      dplyr::inner_join(referencia_2010, by = "ID_SECAO"),
    by = c("ANO_ELEICAO", "NR_TURNO", "ID_SECAO"),
    unmatched = "ignore"
  )

dplyr::glimpse(dados_secao)

# % de seções com coordenada por ano
dados_secao |>
  dplyr::distinct(ANO_ELEICAO, ID_SECAO, NR_LATITUDE) |>
  dplyr::summarise(
    pct_com_coordenada = base::round(100 * base::mean(!base::is.na(NR_LATITUDE)), 1),
    .by = ANO_ELEICAO
  )

# ==============================================================================

# Total de linhas por ano, usado como denominador
total_por_ano <- dados_secao |>
  dplyr::count(ANO_ELEICAO, name = "n_total")

# x: NAs de cada coluna por ano (formato largo), com o total de linhas
x <- dados_secao |>
  dplyr::summarise(
    dplyr::across(
      tidyselect::everything(),
      \(x) base::sum(base::is.na(x))
    ),
    .by = ANO_ELEICAO
  ) |>
  dplyr::left_join(total_por_ano, by = "ANO_ELEICAO") |>
  dplyr::relocate(n_total, .after = ANO_ELEICAO) |>
  dplyr::arrange(ANO_ELEICAO)

dplyr::glimpse(x)

# y: só as colunas com NA (formato longo), com o % que representam no ano
y <- x |>
  tidyr::pivot_longer(
    -c(ANO_ELEICAO, n_total),
    names_to  = "coluna",
    values_to = "n_na"
  ) |>
  dplyr::filter(n_na > 0) |>
  dplyr::mutate(pct_na = base::round(100 * n_na / n_total, 1)) |>
  dplyr::arrange(coluna, ANO_ELEICAO)

dplyr::glimpse(y)
y

# =============================================================================
# PASSO 5: SALVAR E REPRODUTIBILIDADE
# =============================================================================
base::message("PASSO 5: salvando")

readr::write_csv(dados_secao, base::paste0(SAIDA, ".csv"))
base::saveRDS(dados_secao, base::paste0(SAIDA, ".rds"))

utils::sessionInfo()

# Apaga os arquivos brutos de locais de votação (.zip e .csv extraídos),
# mas só depois de conferir que o banco final foi salvo
base::stopifnot(
  base::file.exists(base::paste0(SAIDA, ".csv")),
  base::file.exists(base::paste0(SAIDA, ".rds"))
)

arquivos_brutos <- base::list.files(
  PASTA,
  pattern    = "^eleitorado_local_votacao_\\d{4}\\.(zip|csv)$",
  full.names = TRUE
)

utils::str(arquivos_brutos)

base::unlink(arquivos_brutos)

base::stopifnot(!base::any(base::file.exists(arquivos_brutos)))
base::message("   ", base::length(arquivos_brutos), " arquivos brutos apagados")