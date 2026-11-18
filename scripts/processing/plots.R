# =============================================================================
# UNIVERSIDADE FEDERAL DE PERNAMBUCO - Departamento de Ciência Política
# Disciplina: Partidos Políticos e Eleições
# Arquivo: graficos_reduto_pe.R
#
# OBJETIVO (didático):
#   Uma FUNÇÃO, graficos_reduto(partido, ano), que devolve dois gráficos
#   ESTÁTICOS lado a lado para a eleição de governador em PE:
#     esquerda: superfície 3D da força do partido (x = longitude,
#               y = latitude, z = força);
#     direita:  a projeção dessa superfície no plano (mapa com curvas de
#               nível).
#   A escala de cores e de altura vai do MÍNIMO ao MÁXIMO do próprio
#   partido (e não de 0 a 1), para o relevo de partidos pequenos aparecer.
#   Basta mudar o partido ou o ano para ter outra figura.
#
# SAÍDAS (quando salvar = TRUE):
#   figuras/reduto_<PARTIDO>_<ANO>.png  (300 dpi, fundo branco)
#   figuras/reduto_<PARTIDO>_<ANO>.pdf  (vetorial, cairo_pdf)
#
# DADOS DE ENTRADA: dados/votacao_secao_PE_1998_2022_governador.rds, lido
#   com here::here() (abra o .Rproj do projeto).
#
# CODIFICAÇÃO: UTF-8.   VERSÃO MÍNIMA DO R: 4.1 (pipe nativo |>)
# =============================================================================


# =============================================================================
# GUIA RÁPIDO DE SINTAXE (só o que aparece neste script)
# =============================================================================
# pacote::funcao()    chama a função de um pacote sem library().
# objeto <- valor     atribuição.
# x |> f()            pipe nativo: passa x como 1º argumento de f().
# ^                   potência (p^2).
# %in%                TRUE se o valor está no vetor.
# !                   negação.
# x[condicao]         só os elementos de x em que a condição é TRUE.
# .by = c(...)        agrupamento dentro de summarise()/mutate().
# here::here("a","b") caminho a partir da raiz do projeto (.Rproj).
# function(a, b = 1)  cria uma função; b = 1 é o valor padrão de b.
# match.arg(x)        aceita só um dos valores previstos para o argumento.
# if (a) b else c     escolhe b ou c conforme a condição.
# ggplot() + camada   empilha camadas no ggplot2.
# aes(x = , y = )     liga colunas a eixos e cores.
# ~ { ... }           em wrap_elements(), a "receita" de um gráfico do R
#                     base que o patchwork desenha depois.
# p1 | p2             patchwork: p1 ao lado de p2.
# m[-1, ]             matriz sem a 1ª linha (índice negativo = remover).
# =============================================================================


# =============================================================================
# PASSO 0: CONFIGURAÇÃO E PACOTES
# =============================================================================
base::message("PASSO 0: configuração e pacotes")

ARQUIVO_ENTRADA <- c("dados", "votacao_secao_PE_1998_2022_governador.rds")
PASTA_FIGURAS   <- "figuras"

# 95 = branco, 96 = nulo, 97 e 98 = anulados / apurados em separado
CODIGOS_NAO_VALIDOS <- c(95, 96, 97, 98)

# Chave do local de votação (empresta coordenadas aos anos antigos)
CHAVE_LOCAL <- c("CD_MUNICIPIO", "NR_ZONA", "NR_LOCAL_VOTACAO")

TURNO_ESCOLHIDO <- 1
CODIGO_NORONHA  <- 2605459
CRS_GRAUS       <- 4674

# Padrões da função (podem ser trocados em cada chamada)
SPAN_PADRAO      <- 0.08   # loess: menor = mais picos; maior = mais liso
RESOLUCAO_MALHA  <- 150    # pontos da malha em cada eixo
ANGULO_THETA     <- -30    # giro do 3D em torno do eixo vertical
ANGULO_PHI       <- 30     # inclinação do 3D
EXAGERO_Z        <- 6      # multiplica a altura para o relevo aparecer

pacotes <- c("here", "dplyr", "ggplot2", "sf", "geobr", "patchwork",
             "gridGraphics", "viridisLite")

pacotes_faltando <- base::setdiff(pacotes,
                                  base::rownames(utils::installed.packages()))
if (base::length(pacotes_faltando) > 0) {
  utils::install.packages(pacotes_faltando)
}
utils::str(pacotes)


# =============================================================================
# PASSO 1: LEITURA DO BANCO
# =============================================================================
# FUNÇÃO: here::here() monta o caminho a partir da raiz do projeto;
#         base::do.call() passa os pedaços de ARQUIVO_ENTRADA como
#         argumentos separados.
# -----------------------------------------------------------------------------
base::message("PASSO 1: lendo o banco")

caminho_entrada <- base::do.call(here::here, base::as.list(ARQUIVO_ENTRADA))
base::message("   ", caminho_entrada)

if (!base::file.exists(caminho_entrada)) {
  base::stop("Arquivo não encontrado: ", caminho_entrada,
             "\nRaiz do projeto segundo o here: ", here::here(),
             "\nAbra o .Rproj do projeto ou corrija ARQUIVO_ENTRADA.")
}

dados_secao <- base::readRDS(caminho_entrada)
dplyr::glimpse(dados_secao)
base::stopifnot(base::nrow(dados_secao) > 0)

colunas_necessarias <- c("ANO_ELEICAO", "NR_TURNO", "ID_SECAO", "NR_VOTAVEL",
                         "SG_PARTIDO", "QT_VOTOS",
                         "NR_LATITUDE", "NR_LONGITUDE", CHAVE_LOCAL)
colunas_ausentes <- base::setdiff(colunas_necessarias, base::names(dados_secao))

if (base::length(colunas_ausentes) > 0) {
  base::stop(
    "Colunas ausentes: ", base::paste(colunas_ausentes, collapse = ", "),
    "\nColunas disponíveis: ",
    base::paste(base::names(dados_secao), collapse = ", ")
  )
}


# =============================================================================
# PASSO 2: VOTOS VÁLIDOS, PERCENTUAL E PERCENTUAL AO QUADRADO
# =============================================================================
# O QUE: em cada seção, por ano e turno:
#   VOTO_VALIDO, VOTOS_VALIDOS, PCT_VOTOS_VALIDOS (0 a 100) e
#   PCT_VOTOS_SQR = (PCT_VOTOS_VALIDOS / 100)^2 (proporção ao quadrado).
# FUNÇÃO: dplyr::mutate(.by = ...) calcula por grupo sem resumir as linhas.
# -----------------------------------------------------------------------------
base::message("PASSO 2: votos válidos e percentuais")

dados_secao <- dados_secao |>
  dplyr::mutate(
    VOTO_VALIDO   = !NR_VOTAVEL %in% CODIGOS_NAO_VALIDOS,
    VOTOS_VALIDOS = base::sum(QT_VOTOS[VOTO_VALIDO]),
    .by = c(ANO_ELEICAO, NR_TURNO, ID_SECAO)
  ) |>
  dplyr::mutate(
    PCT_VOTOS_VALIDOS = dplyr::if_else(VOTO_VALIDO & VOTOS_VALIDOS > 0,
                                       100 * QT_VOTOS / VOTOS_VALIDOS,
                                       NA_real_),
    PCT_VOTOS_SQR = (PCT_VOTOS_VALIDOS / 100)^2
  )

dplyr::glimpse(dados_secao)
dados_secao |>
  dplyr::select(PCT_VOTOS_VALIDOS, PCT_VOTOS_SQR) |>
  base::summary()

base::stopifnot(base::all(dados_secao$PCT_VOTOS_SQR <= 1, na.rm = TRUE))

secoes_com_erro <- dados_secao |>
  dplyr::filter(VOTO_VALIDO, VOTOS_VALIDOS > 0) |>
  dplyr::summarise(SOMA_PCT = base::sum(PCT_VOTOS_VALIDOS),
                   .by = c(ANO_ELEICAO, NR_TURNO, ID_SECAO)) |>
  dplyr::filter(base::abs(SOMA_PCT - 100) > 0.01)

if (base::nrow(secoes_com_erro) > 0) {
  base::stop("Percentuais não somam 100 em ", base::nrow(secoes_com_erro),
             " seções, ex.: ",
             base::paste(utils::head(secoes_com_erro$ID_SECAO), collapse = ", "))
}


# =============================================================================
# PASSO 3: COORDENADAS PARA TODOS OS ANOS
# =============================================================================
# O QUE: coordenada mais recente de cada local de votação, emprestada a
#        todos os anos (os anos antigos vêm sem coordenada do TSE).
# CUIDADO: o código do local pode mudar entre eleições; veja cobertura_anos.
# -----------------------------------------------------------------------------
base::message("PASSO 3: coordenadas")

dados_limpos <- dados_secao |>
  dplyr::filter(NR_TURNO == TURNO_ESCOLHIDO) |>
  dplyr::mutate(
    NR_LATITUDE  = dplyr::if_else(NR_LATITUDE  == -1, NA_real_, NR_LATITUDE),
    NR_LONGITUDE = dplyr::if_else(NR_LONGITUDE == -1, NA_real_, NR_LONGITUDE)
  )

coordenadas_local <- dados_limpos |>
  dplyr::filter(!base::is.na(NR_LATITUDE), !base::is.na(NR_LONGITUDE)) |>
  dplyr::arrange(dplyr::desc(ANO_ELEICAO)) |>
  dplyr::distinct(dplyr::across(dplyr::all_of(CHAVE_LOCAL)), .keep_all = TRUE) |>
  dplyr::select(dplyr::all_of(CHAVE_LOCAL),
                LATITUDE = NR_LATITUDE, LONGITUDE = NR_LONGITUDE)
dplyr::glimpse(coordenadas_local)

if (base::nrow(coordenadas_local) == 0) {
  base::stop("Nenhum local de votação tem coordenada em nenhum ano.")
}

validos_coordenados <- dados_limpos |>
  dplyr::filter(VOTO_VALIDO) |>
  dplyr::left_join(coordenadas_local, by = CHAVE_LOCAL)

cobertura_anos <- validos_coordenados |>
  dplyr::summarise(
    PCT_VOTOS_COM_COORDENADA = 100 * base::sum(
      QT_VOTOS[!base::is.na(LATITUDE)]) / base::sum(QT_VOTOS),
    .by = ANO_ELEICAO
  ) |>
  dplyr::arrange(ANO_ELEICAO)
cobertura_anos

validos_coordenados <- dplyr::filter(validos_coordenados,
                                     !base::is.na(LATITUDE))
dplyr::glimpse(validos_coordenados)
base::stopifnot(base::nrow(validos_coordenados) > 0)

# Total de votos válidos de cada local (denominador da proporção)
locais <- validos_coordenados |>
  dplyr::summarise(
    VOTOS_VALIDOS_LOCAL = base::sum(QT_VOTOS),
    LATITUDE  = dplyr::first(LATITUDE),
    LONGITUDE = dplyr::first(LONGITUDE),
    .by = c(ANO_ELEICAO, dplyr::all_of(CHAVE_LOCAL))
  )
dplyr::glimpse(locais)

# Partidos e anos disponíveis, para consulta
partidos_anos <- validos_coordenados |>
  dplyr::count(ANO_ELEICAO, SG_PARTIDO, wt = QT_VOTOS, name = "VOTOS") |>
  dplyr::arrange(ANO_ELEICAO, dplyr::desc(VOTOS))
partidos_anos


# =============================================================================
# PASSO 4: CONTORNO DE PE
# =============================================================================
# FUNÇÃO: geobr::read_municipality() baixa a malha do IBGE;
#         sf::st_intersects() marca os locais que caem dentro de PE.
# -----------------------------------------------------------------------------
base::message("PASSO 4: contorno de PE")

contorno_pe <- geobr::read_municipality(code_muni = "PE", year = 2022,
                                        showProgress = TRUE) |>
  dplyr::filter(code_muni != CODIGO_NORONHA) |>
  sf::st_transform(CRS_GRAUS) |>
  sf::st_make_valid() |>
  sf::st_union() |>
  sf::st_as_sf()

sf::st_crs(contorno_pe)$input
sf::st_geometry_type(contorno_pe)

dentro_do_contorno <- function(tabela) {
  pontos <- sf::st_as_sf(tabela, coords = c("LONGITUDE", "LATITUDE"),
                         crs = CRS_GRAUS)
  base::lengths(sf::st_intersects(pontos, contorno_pe)) > 0
}

locais <- locais |>
  dplyr::mutate(DENTRO_PE = dentro_do_contorno(locais)) |>
  dplyr::filter(DENTRO_PE)
dplyr::glimpse(locais)

# =============================================================================
# PASSO 5: MALHA REGULAR
# =============================================================================
# O QUE: grade de RESOLUCAO_MALHA x RESOLUCAO_MALHA pontos sobre PE.
# FUNÇÃO: base::expand.grid() combina todos os valores de x e de y
#         (a longitude varia mais rápido).
# -----------------------------------------------------------------------------
base::message("PASSO 5: malha regular")

limites_pe <- sf::st_bbox(contorno_pe)
vetor_longitude <- base::seq(limites_pe[["xmin"]], limites_pe[["xmax"]],
                             length.out = RESOLUCAO_MALHA)
vetor_latitude  <- base::seq(limites_pe[["ymin"]], limites_pe[["ymax"]],
                             length.out = RESOLUCAO_MALHA)

malha <- base::expand.grid(LONGITUDE = vetor_longitude,
                           LATITUDE  = vetor_latitude)
malha_dentro_pe <- dentro_do_contorno(malha)
dplyr::glimpse(malha)
utils::str(malha_dentro_pe)


# =============================================================================
# PASSO 6: FUNÇÕES AUXILIARES
# =============================================================================
# forca_partido_ano(): proporção (ou proporção ao quadrado) do partido em
#   cada local num ano. Locais sem voto no partido entram com ZERO.
# superficie_partido_ano(): loess na malha; devolve um vetor z (NA fora
#   de PE), na mesma ordem das linhas de malha.
# -----------------------------------------------------------------------------
base::message("PASSO 6: funções auxiliares")

forca_partido_ano <- function(partido_busca, ano, usar_quadrado) {
  votos_partido <- validos_coordenados |>
    dplyr::filter(ANO_ELEICAO == ano,
                  base::grepl(partido_busca, SG_PARTIDO)) |>
    dplyr::summarise(VOTOS_PARTIDO = base::sum(QT_VOTOS),
                     .by = dplyr::all_of(CHAVE_LOCAL))
  
  if (base::nrow(votos_partido) == 0) {
    anos_partido <- partidos_anos |>
      dplyr::filter(base::grepl(partido_busca, SG_PARTIDO)) |>
      dplyr::pull(ANO_ELEICAO) |>
      base::unique()
    base::stop("Sem votos para '", partido_busca, "' em ", ano,
               ". Anos com esse partido: ",
               base::paste(anos_partido, collapse = ", "),
               "\nVeja a tabela partidos_anos para as siglas.")
  }
  
  locais |>
    dplyr::filter(ANO_ELEICAO == ano) |>
    dplyr::left_join(votos_partido, by = CHAVE_LOCAL) |>
    dplyr::mutate(
      VOTOS_PARTIDO = dplyr::coalesce(VOTOS_PARTIDO, 0),
      PROPORCAO     = VOTOS_PARTIDO / VOTOS_VALIDOS_LOCAL,
      Z             = if (usar_quadrado) PROPORCAO^2 else PROPORCAO
    )
}

superficie_partido_ano <- function(forca, span) {
  modelo <- stats::loess(Z ~ LONGITUDE + LATITUDE, data = forca,
                         span = span, degree = 2)
  z <- base::as.numeric(stats::predict(modelo, newdata = malha))
  z <- base::pmin(base::pmax(z, 0), 1)   # a proporção fica entre 0 e 1
  z[!malha_dentro_pe] <- NA              # recorta o formato de PE
  z
}

# Inspeção rápida com o partido mais votado do ano mais recente
ano_teste     <- base::max(partidos_anos$ANO_ELEICAO)
partido_teste <- partidos_anos |>
  dplyr::filter(ANO_ELEICAO == ano_teste) |>
  dplyr::slice(1) |>
  dplyr::pull(SG_PARTIDO)

forca_teste <- forca_partido_ano(base::paste0("^", partido_teste, "$"),
                                 ano_teste, usar_quadrado = TRUE)
dplyr::glimpse(forca_teste)
base::summary(forca_teste$Z)


# =============================================================================
# PASSO 7: A FUNÇÃO PRINCIPAL - graficos_reduto()
# =============================================================================
# O QUE: devolve (e, se pedido, salva) a figura 3D | 2D de um partido num ano.
# ARGUMENTOS:
#   partido_busca  sigla ou expressão regular ("PSB", "SOLIDARIEDADE|^SD$")
#   ano            ano da eleição
#   usar_quadrado  TRUE = proporção ao quadrado; FALSE = proporção
#   escala         "partido": mínimo e máximo do partido em TODOS os seus
#                  anos (compara anos do mesmo partido);
#                  "ano": mínimo e máximo só deste ano (mais contraste)
#   ÂNGULO DO 3D:
#     theta        giro em torno do eixo vertical, em graus
#                  (0 = olhando do sul; 90 = do leste; -90 = do oeste)
#     phi          inclinação, em graus (0 = de lado; 90 = de cima)
#     exagero_z    multiplica a altura (relevo mais ou menos alto)
#     distancia    distância do observador (maior = mais longe)
#     perspectiva  força da perspectiva (1 = normal; valores altos, como
#                  100, deixam a vista quase ortográfica, sem fuga)
#   span: suavização do loess; turno: só para o título;
#   salvar: grava PNG e PDF em pasta_figuras
# FUNÇÃO: graphics::persp() desenha a superfície 3D; patchwork::
#   wrap_elements() a transforma em painel ao lado do mapa do ggplot2.
#   persp() quer z com LINHAS = longitude, por isso base::t().
# -----------------------------------------------------------------------------
base::message("PASSO 7: função graficos_reduto()")

graficos_reduto <- function(partido_busca,
                            ano,
                            usar_quadrado = TRUE,
                            escala        = c("partido", "ano"),
                            span          = 0.08,
                            theta         = -30,
                            phi           = 30,
                            exagero_z     = 6,
                            distancia     = 1.7,
                            perspectiva   = 1,
                            salvar        = TRUE,
                            pasta_figuras = "figuras") {
  
  escala <- base::match.arg(escala)
  nome_partido <- base::gsub("[^A-Za-z]", "", partido_busca)
  n <- base::length(vetor_longitude)          # resolução da malha
  
  # Sigla como aparece no banco naquele ano (ex.: "SOLIDARIEDADE")
  sigla_ano <- partidos_anos |>
    dplyr::filter(ANO_ELEICAO == ano, base::grepl(partido_busca, SG_PARTIDO)) |>
    dplyr::pull(SG_PARTIDO) |>
    base::unique() |>
    base::paste(collapse = "/")
  
  # 1. Superfície do ano pedido
  forca_ano <- forca_partido_ano(partido_busca, ano, usar_quadrado)
  z_ano <- superficie_partido_ano(forca_ano, span)
  
  # 2. Limites da escala: do próprio partido
  if (escala == "partido") {
    anos_partido <- partidos_anos |>
      dplyr::filter(base::grepl(partido_busca, SG_PARTIDO)) |>
      dplyr::pull(ANO_ELEICAO) |>
      base::unique()
    
    z_todos_anos <- base::lapply(anos_partido, function(a) {
      superficie_partido_ano(forca_partido_ano(partido_busca, a,
                                               usar_quadrado), span)
    })
    limites_z <- base::range(base::unlist(z_todos_anos), na.rm = TRUE)
  } else {
    limites_z <- base::range(z_ano, na.rm = TRUE)
  }
  base::stopifnot(base::all(base::is.finite(limites_z)),
                  limites_z[2] > limites_z[1])
  
  rotulo_z <- if (usar_quadrado) "Força (p²)" else "Proporção de votos válidos"
  
  # Piso do 3D: um pouco abaixo do mínimo, para o mapa ficar sob a superfície
  z_piso <- limites_z[1] - 0.15 * base::diff(limites_z)
  
  # Contorno de PE como linhas com NA entre as partes ("levanta a caneta")
  linhas_pe <- contorno_pe |>
    sf::st_boundary() |>
    sf::st_cast("MULTILINESTRING") |>
    sf::st_cast("LINESTRING") |>
    sf::st_coordinates() |>
    base::as.data.frame() |>
    base::split(~ L1) |>
    base::lapply(function(parte) {
      base::rbind(parte[, c("X", "Y")], base::data.frame(X = NA, Y = NA))
    }) |>
    dplyr::bind_rows()
  
  # 3. Painel 3D (R base), sem título, com o mapa de PE no piso
  matriz_z <- base::matrix(z_ano, nrow = n, ncol = n, byrow = TRUE) |>
    base::t()                                 # linhas = longitude
  z_facela <- (matriz_z[-1, -1] + matriz_z[-1, -n] +
                 matriz_z[-n, -1] + matriz_z[-n, -n]) / 4
  cor_facela <- viridisLite::viridis(100)[base::cut(
    z_facela,
    breaks = base::seq(limites_z[1], limites_z[2], length.out = 101),
    include.lowest = TRUE
  )]
  
  painel_3d <- patchwork::wrap_elements(full = ~ {
    graphics::par(mar = c(1, 1, 0, 1))
    # persp() devolve a matriz de transformação 3D -> 2D
    transformacao <- graphics::persp(
      x = vetor_longitude, y = vetor_latitude, z = matriz_z,
      zlim = c(z_piso, limites_z[2]), col = cor_facela, border = NA,
      theta = theta, phi = phi, r = distancia, d = perspectiva,
      scale = FALSE, expand = exagero_z,
      ticktype = "detailed", cex.axis = 0.6, nticks = 4,
      xlab = "Longitude", ylab = "Latitude", zlab = rotulo_z
    )
    # grDevices::trans3d() leva cada ponto (x, y, z_piso) para a tela
    graphics::lines(
      grDevices::trans3d(linhas_pe$X, linhas_pe$Y, z_piso, transformacao),
      col = "grey20", lwd = 1.2
    )
  })
  
  # 4. Painel 2D (projeção no plano), com título "SIGLA, ano"
  superficie_2d <- malha |>
    dplyr::mutate(Z = z_ano) |>
    dplyr::filter(!base::is.na(Z))
  
  # Posição do título: canto superior esquerdo de PE, um pouco acima do mapa
  limites_mapa <- sf::st_bbox(contorno_pe)
  
  painel_2d <- ggplot2::ggplot() +
    ggplot2::geom_raster(data = superficie_2d,
                         ggplot2::aes(x = LONGITUDE, y = LATITUDE, fill = Z)) +
    ggplot2::geom_contour(data = superficie_2d,
                          ggplot2::aes(x = LONGITUDE, y = LATITUDE, z = Z),
                          color = "white", linewidth = 0.2, bins = 10) +
    ggplot2::geom_sf(data = contorno_pe, fill = NA, color = "grey30") +
    # Título colado no mapa: "SIGLA, ano"
    ggplot2::annotate(
      "text",
      x = limites_mapa[["xmin"]],
      y = limites_mapa[["ymax"]] + 0.25,
      label = base::paste0(sigla_ano, ", ", ano),
      hjust = 0, vjust = 0, size = 5, fontface = "bold"
    ) +
    ggplot2::scale_fill_viridis_c(name = rotulo_z, limits = limites_z) +
    # clip = "off" deixa o texto aparecer mesmo fora da área do mapa
    ggplot2::coord_sf(clip = "off") +
    ggplot2::labs(x = "Longitude", y = "Latitude") +
    ggplot2::theme_minimal()
  
  # 5. Figura final: só os dois painéis (sem título geral nem fonte)
  figura <- (painel_3d | painel_2d) +
    patchwork::plot_layout(widths = c(1, 1.3))
  
  if (salvar) {
    base::dir.create(here::here(pasta_figuras), showWarnings = FALSE)
    arquivo <- here::here(pasta_figuras,
                          base::paste0("reduto_", nome_partido, "_", ano))
    ggplot2::ggsave(base::paste0(arquivo, ".png"), figura,
                    width = 14, height = 6, dpi = 300, bg = "white")
    ggplot2::ggsave(base::paste0(arquivo, ".pdf"), figura,
                    width = 14, height = 6, device = grDevices::cairo_pdf)
    base::stopifnot(base::file.exists(base::paste0(arquivo, ".png")))
    base::message("   salvo em ", arquivo, ".png / .pdf")
  }
  
  figura
}

# =============================================================================
# PASSO 8: USANDO A FUNÇÃO
# =============================================================================
# Mude o partido e o ano à vontade. Consulte partidos_anos para as siglas.
# -----------------------------------------------------------------------------
base::message("PASSO 8: exemplos")

graficos_reduto("SOLIDARIEDADE|^SD$", 2022)

graficos_reduto("^PSDB$", 2022)
graficos_reduto("^PL$", 2022)
graficos_reduto("^PSB$", 2022)
graficos_reduto("UNIÃO|^UNIAO$", 2022)
graficos_reduto("SOLIDARIEDADE|^SD$", 2022)
graficos_reduto("^PCB$", 2022)


# Outros exemplos (tire o # para rodar):
# graficos_reduto("^PSB$", 2018)
# graficos_reduto("^PSB$", 2022, escala = "ano")
# graficos_reduto("SOLIDARIEDADE|^SD$", 2022, usar_quadrado = FALSE)
# graficos_reduto("^PT$", 2006, theta = 20, exagero_z = 10)
# Vista quase ortográfica, olhando do sudoeste, mais de cima:
graficos_reduto("SOLIDARIEDADE|^SD$", 2022, theta = -40, phi = 45, perspectiva = 100, salvar = FALSE)

# =============================================================================
# PASSO 9: PAINEL 2010-2022 (1 coluna x 4 linhas) COM PATCHWORK
# =============================================================================
# O QUE: roda graficos_reduto() para cada ano SEM salvar (salvar = FALSE),
#        guarda cada figura em uma lista e empilha as quatro numa coluna.
# POR QUÊ: salvar = FALSE evita gravar um arquivo por ano; só a figura final
#          é salva.
# FUNÇÃO: patchwork::wrap_plots(lista, ncol = 1) empilha as figuras da lista.
# -----------------------------------------------------------------------------
base::message("PASSO 9: painel de anos")

g2010 <- graficos_reduto("^PSB$", 2010, theta = -40, phi = 45,
                         perspectiva = 100, salvar = FALSE)
g2014 <- graficos_reduto("^PSB$", 2014, theta = -40, phi = 45,
                         perspectiva = 100, salvar = FALSE)
g2018 <- graficos_reduto("^PSB$", 2018, theta = -40, phi = 45,
                         perspectiva = 100, salvar = FALSE)
g2022 <- graficos_reduto("^PSB$", 2022, theta = -40, phi = 45,
                         perspectiva = 100, salvar = FALSE)

painel <- g2010 / g2014 / g2018 / g2022    # "/" empilha: uma coluna
painel

ggplot2::ggsave(here::here("figuras", "painel_PSB_2010_2022.png"),
                painel, width = 14, height = 22, dpi = 300, bg = "white")

# -------------------------------------
# TESTE 2

g2010 <- graficos_reduto("^PSB$", 2010, salvar = FALSE)
g2014 <- graficos_reduto("^PSB$", 2014, salvar = FALSE)
g2018 <- graficos_reduto("^PSB$", 2018, salvar = FALSE)
g2022 <- graficos_reduto("^PSB$", 2022, salvar = FALSE)

painel <- g2010 / g2014 / g2018 / g2022

# Salve direto: o painel Plots do RStudio é pequeno demais para 4 gráficos 3D
ggplot2::ggsave(here::here("figuras", "painel_PSB_2010_2022(1).png"),
                painel, width = 14, height = 22, dpi = 300, bg = "white")

utils::browseURL(here::here("figuras", "painel_PSB_2010_2022.png"))

# =============================================================================
# PASSO 10: ANIMAÇÃO DO MAPA (gganimate) - animar_reduto()
# =============================================================================
# O QUE: gera um GIF em que cada quadro é um ano em que o partido disputou;
#        só a projeção no plano (o mapa da direita) é animada.
# POR QUÊ: mostra o reduto mudando de lugar ao longo da série.
# FUNÇÃO: gganimate::transition_manual(ANO_ELEICAO) troca o quadro a cada
#         ano, sem interpolar (cada quadro mostra só os dados daquele ano);
#         gganimate::animate() desenha o GIF; gganimate::anim_save() salva.
#         A escala vai do mínimo ao máximo do partido em todos os anos.
# PRECISA DE: malha, contorno_pe, partidos_anos, forca_partido_ano(),
#             superficie_partido_ano() (passos anteriores).
# -----------------------------------------------------------------------------
base::message("PASSO 10: animação do mapa")

pacotes_animacao <- c("gganimate", "gifski")
faltando <- base::setdiff(pacotes_animacao,
                          base::rownames(utils::installed.packages()))
if (base::length(faltando) > 0) utils::install.packages(faltando)

animar_reduto <- function(partido_busca,
                          usar_quadrado   = TRUE,
                          span            = 0.08,
                          segundos_por_ano = 1.5,
                          largura         = 1600,
                          altura          = 700,
                          salvar          = TRUE,
                          pasta_figuras   = "figuras") {
  
  nome_partido <- base::gsub("[^A-Za-z]", "", partido_busca)
  
  # 1. Anos e siglas do partido na série
  siglas_anos <- partidos_anos |>
    dplyr::filter(base::grepl(partido_busca, SG_PARTIDO)) |>
    dplyr::summarise(SIGLA = base::paste(base::unique(SG_PARTIDO),
                                         collapse = "/"),
                     .by = ANO_ELEICAO) |>
    dplyr::arrange(ANO_ELEICAO)
  
  if (base::nrow(siglas_anos) < 2) {
    base::stop("O partido '", partido_busca, "' aparece em ",
               base::nrow(siglas_anos), " ano(s); a animação precisa de 2+.")
  }
  base::message("   anos: ", base::paste(siglas_anos$ANO_ELEICAO,
                                         collapse = ", "))
  
  # 2. Uma superfície por ano, empilhadas numa tabela só
  superficies <- base::lapply(siglas_anos$ANO_ELEICAO, function(ano) {
    forca <- forca_partido_ano(partido_busca, ano, usar_quadrado)
    malha |>
      dplyr::mutate(Z = superficie_partido_ano(forca, span),
                    ANO_ELEICAO = ano)
  }) |>
    dplyr::bind_rows() |>
    dplyr::filter(!base::is.na(Z))
  
  dplyr::glimpse(superficies)
  
  # Escala do partido: mínimo e máximo em todos os anos
  limites_z <- base::range(superficies$Z)
  rotulo_z  <- if (usar_quadrado) "Força (p²)" else "Proporção de votos válidos"
  
  # 3. Título "SIGLA, ano" colado no mapa (um rótulo por ano)
  limites_mapa <- sf::st_bbox(contorno_pe)
  titulos <- siglas_anos |>
    dplyr::mutate(ROTULO = base::paste0(SIGLA, ", ", ANO_ELEICAO),
                  X = limites_mapa[["xmin"]],
                  Y = limites_mapa[["ymax"]] + 0.25)
  
  # 4. O mapa (mesma lógica do painel da direita) + transição por ano
  mapa_animado <- ggplot2::ggplot() +
    ggplot2::geom_raster(data = superficies,
                         ggplot2::aes(x = LONGITUDE, y = LATITUDE, fill = Z)) +
    ggplot2::geom_contour(data = superficies,
                          ggplot2::aes(x = LONGITUDE, y = LATITUDE, z = Z),
                          color = "white", linewidth = 0.2, bins = 10) +
    ggplot2::geom_sf(data = contorno_pe, fill = NA, color = "grey30") +
    ggplot2::geom_text(data = titulos,
                       ggplot2::aes(x = X, y = Y, label = ROTULO),
                       hjust = 0, vjust = 0, size = 6, fontface = "bold") +
    ggplot2::scale_fill_viridis_c(name = rotulo_z, limits = limites_z) +
    ggplot2::coord_sf(clip = "off") +
    ggplot2::labs(x = "Longitude", y = "Latitude") +
    ggplot2::theme_minimal(base_size = 14) +
    gganimate::transition_manual(ANO_ELEICAO)
  
  # 5. Desenha o GIF
  n_anos <- base::nrow(siglas_anos)
  gif <- gganimate::animate(
    mapa_animado,
    nframes  = n_anos,
    fps      = 1 / segundos_por_ano,
    width    = largura, height = altura, res = 150,
    renderer = gganimate::gifski_renderer()
  )
  
  if (salvar) {
    base::dir.create(here::here(pasta_figuras), showWarnings = FALSE)
    arquivo <- here::here(pasta_figuras,
                          base::paste0("animacao_", nome_partido, ".gif"))
    gganimate::anim_save(arquivo, animation = gif)
    base::stopifnot(base::file.exists(arquivo))
    base::message("   salvo em ", arquivo)
  }
  
  gif
}

# Uso
animar_reduto("^PT$")

# =============================================================================
# PASSO FINAL: REPRODUTIBILIDADE
# =============================================================================
base::message("PASSO FINAL: informações da sessão")
utils::sessionInfo()

# =============================================================================
# ANEXO: FUNÇÃO PLOTANDO GRAFICOS EM DUPLA
# =============================================================================

# ANOS DE 2022 ----------------------------------------------------------------

dupla_redutos <- function(partido_cima, partido_baixo, ano, nome_arquivo) {
  figura <- graficos_reduto(partido_cima,  ano, salvar = FALSE) /
    graficos_reduto(partido_baixo, ano, salvar = FALSE)
  
  arquivo <- here::here("figuras", base::paste0(nome_arquivo, ".png"))
  base::dir.create(here::here("figuras"), showWarnings = FALSE)
  ggplot2::ggsave(arquivo, figura, width = 14, height = 12, dpi = 300, bg = "white")
  base::message("   salvo em ", arquivo)
  
  figura
}

dupla_redutos("SOLIDARIEDADE|^SD$", "^PSDB$", 2022, "redutos_SD_PSDB_2022")
dupla_redutos("^PL$", "^PSB$", 2022, "redutos_PL_PSB_2022")
dupla_redutos("UNIÃO|^UNIAO$", "^PCB$", 2022, "redutos_UNIAO_PCB_2022")

# ANOS DE 1998 ----------------------------------------------------------------

dupla_redutos <- function(partido_cima, partido_baixo, ano, nome_arquivo) {
  figura <- graficos_reduto(partido_cima,  ano, salvar = FALSE) /
    graficos_reduto(partido_baixo, ano, salvar = FALSE)
  
  arquivo <- here::here("figuras", base::paste0(nome_arquivo, ".png"))
  base::dir.create(here::here("figuras"), showWarnings = FALSE)
  ggplot2::ggsave(arquivo, figura, width = 14, height = 12, dpi = 300, bg = "white")
  base::message("   salvo em ", arquivo)
  
  figura
}

dupla_redutos("^PMDB$", "^PSB$", 1998, "redutos_PMDB_PSB_1998")
dupla_redutos("^PSDB$", "^PSC$", 1998, "redutos_PSDB_PSC_1998")
dupla_redutos("^PSTU$", "^PSN$", 1998, "redutos_PSTU_PSN_1998")

# =============================================================================
# PASSO 11: GRADE TODOS OS PARTIDOS (linhas) x 1998-2022 (colunas) - força p²
# =============================================================================
# O QUE: um mapa de força (p²) por partido e ano; quadro em branco quando o
#        partido não disputou (ou quando o loess não consegue ajustar).
# POR QUÊ: recria locais e malha aqui, porque outros scripts usam os mesmos
#          nomes e podem tê-los sobrescrito.
# FUNÇÃO: ggplot2::facet_grid(drop = FALSE) mantém as células vazias;
#         base::Map() aplica superficie_celula() a cada par partido-ano.
# PRECISA DE: dados_limpos, contorno_pe, CHAVE_LOCAL, dentro_do_contorno(),
#             forca_partido_ano(), superficie_partido_ano() (passos 0 a 6).
# -----------------------------------------------------------------------------
base::message("PASSO 11: grade partidos x anos")

ANOS_GRADE <- c(1998, 2002, 2006, 2010, 2014, 2018, 2022)
SPAN_GRADE <- 0.08
RESOLUCAO  <- 150

# 1. Coordenadas por local de votação (a partir de dados_limpos)
coordenadas_local <- dados_limpos |>
  dplyr::filter(!base::is.na(NR_LATITUDE), !base::is.na(NR_LONGITUDE)) |>
  dplyr::arrange(dplyr::desc(ANO_ELEICAO)) |>
  dplyr::distinct(dplyr::across(dplyr::all_of(CHAVE_LOCAL)), .keep_all = TRUE) |>
  dplyr::select(dplyr::all_of(CHAVE_LOCAL),
                LATITUDE = NR_LATITUDE, LONGITUDE = NR_LONGITUDE)
dplyr::glimpse(coordenadas_local)

validos_coordenados <- dados_limpos |>
  dplyr::filter(VOTO_VALIDO, !base::is.na(SG_PARTIDO)) |>
  dplyr::left_join(coordenadas_local, by = CHAVE_LOCAL) |>
  dplyr::filter(!base::is.na(LATITUDE))
dplyr::glimpse(validos_coordenados)

# 2. Locais de votação dentro de PE (denominador da proporção)
locais <- validos_coordenados |>
  dplyr::summarise(
    VOTOS_VALIDOS_LOCAL = base::sum(QT_VOTOS),
    LATITUDE  = dplyr::first(LATITUDE),
    LONGITUDE = dplyr::first(LONGITUDE),
    .by = c(ANO_ELEICAO, dplyr::all_of(CHAVE_LOCAL))
  )
locais <- dplyr::filter(locais, dentro_do_contorno(locais))
dplyr::glimpse(locais)

partidos_anos <- validos_coordenados |>
  dplyr::count(ANO_ELEICAO, SG_PARTIDO, wt = QT_VOTOS, name = "VOTOS")

# 3. Malha regular sobre PE
limites_pe <- sf::st_bbox(contorno_pe)
vetor_longitude <- base::seq(limites_pe[["xmin"]], limites_pe[["xmax"]],
                             length.out = RESOLUCAO)
vetor_latitude  <- base::seq(limites_pe[["ymin"]], limites_pe[["ymax"]],
                             length.out = RESOLUCAO)
malha <- base::expand.grid(LONGITUDE = vetor_longitude,
                           LATITUDE  = vetor_latitude)
malha_dentro_pe <- dentro_do_contorno(malha)

base::stopifnot(
  base::all(c("LONGITUDE", "LATITUDE") %in% base::names(locais)),
  base::all(c("LONGITUDE", "LATITUDE") %in% base::names(malha))
)

# 4. Partidos (linhas, do mais ao menos votado) e combinações existentes
ordem_partidos <- partidos_anos |>
  dplyr::filter(ANO_ELEICAO %in% ANOS_GRADE) |>
  dplyr::count(SG_PARTIDO, wt = VOTOS, sort = TRUE) |>
  dplyr::pull(SG_PARTIDO)
utils::str(ordem_partidos)

combinacoes <- partidos_anos |>
  dplyr::filter(ANO_ELEICAO %in% ANOS_GRADE) |>
  dplyr::distinct(SG_PARTIDO, ANO_ELEICAO)
dplyr::glimpse(combinacoes)

# 5. Superfície de uma célula (partido x ano); NULL se falhar
superficie_celula <- function(partido, ano) {
  base::message("   ", partido, " ", ano)
  base::tryCatch({
    forca <- forca_partido_ano(base::paste0("^", partido, "$"), ano,
                               usar_quadrado = TRUE)
    z <- superficie_partido_ano(forca, span = SPAN_GRADE)
    base::data.frame(LONGITUDE   = malha$LONGITUDE,
                     LATITUDE    = malha$LATITUDE,
                     Z           = z,
                     SG_PARTIDO  = partido,
                     ANO_ELEICAO = ano)
  }, error = function(erro) {
    base::message("   ! ", partido, " ", ano, " em branco: ",
                  base::conditionMessage(erro))
    NULL
  })
}

# Teste rápido antes de rodar tudo
dplyr::glimpse(superficie_celula(ordem_partidos[1],
                                 base::max(combinacoes$ANO_ELEICAO)))

superficies_grade <- base::Map(superficie_celula,
                               combinacoes$SG_PARTIDO,
                               combinacoes$ANO_ELEICAO) |>
  dplyr::bind_rows()

if (!"Z" %in% base::names(superficies_grade)) {
  base::stop("Nenhuma superfície foi calculada; veja as mensagens acima.")
}

superficies_grade <- superficies_grade |>
  dplyr::filter(!base::is.na(Z)) |>
  dplyr::mutate(
    SG_PARTIDO  = base::factor(SG_PARTIDO,  levels = ordem_partidos),
    ANO_ELEICAO = base::factor(ANO_ELEICAO, levels = ANOS_GRADE)
  )
dplyr::glimpse(superficies_grade)
dplyr::count(superficies_grade, SG_PARTIDO, ANO_ELEICAO)

# 6. Contorno só nas células com dados (as vazias ficam em branco)
contorno_grade <- superficies_grade |>
  dplyr::distinct(SG_PARTIDO, ANO_ELEICAO) |>
  dplyr::cross_join(contorno_pe) |>
  sf::st_as_sf()

# 7. A grade
grade <- ggplot2::ggplot() +
  ggplot2::geom_raster(data = superficies_grade,
                       ggplot2::aes(x = LONGITUDE, y = LATITUDE, fill = Z)) +
  ggplot2::geom_contour(data = superficies_grade,
                        ggplot2::aes(x = LONGITUDE, y = LATITUDE, z = Z),
                        color = "white", linewidth = 0.1, bins = 8) +
  ggplot2::geom_sf(data = contorno_grade, fill = NA, color = "grey30",
                   linewidth = 0.2) +
  ggplot2::scale_fill_viridis_c(name = "Força (p²)") +
  ggplot2::facet_grid(SG_PARTIDO ~ ANO_ELEICAO, drop = FALSE, switch = "y") +
  ggplot2::labs(x = NULL, y = NULL) +
  ggplot2::theme_void(base_size = 11) +
  ggplot2::theme(
    strip.text        = ggplot2::element_text(face = "bold", size = 12),
    strip.text.y.left = ggplot2::element_text(angle = 0, face = "bold"),
    legend.position   = "top",
    legend.key.width  = ggplot2::unit(4, "cm"),
    panel.spacing     = ggplot2::unit(0.3, "lines"),
    plot.background   = ggplot2::element_rect(fill = "white", color = NA)
  )

# 8. Salvar (PDF vetorial para zoom; PNG 300 dpi)
largura <- 4.0 * base::length(ANOS_GRADE) + 2
altura  <- 1.5 * base::length(ordem_partidos) + 2
utils::str(c(largura = largura, altura = altura))

base::dir.create(here::here("figuras"), showWarnings = FALSE)
arquivo_grade <- here::here("figuras", "grade_partidos_1998_2022")

ggplot2::ggsave(base::paste0(arquivo_grade, ".pdf"), grade,
                width = largura, height = altura, limitsize = FALSE,
                device = grDevices::cairo_pdf)
ggplot2::ggsave(base::paste0(arquivo_grade, ".png"), grade,
                width = largura, height = altura, limitsize = FALSE,
                dpi = 300, bg = "white")

base::stopifnot(base::file.exists(base::paste0(arquivo_grade, ".pdf")))
utils::browseURL(base::paste0(arquivo_grade, ".pdf"))