# =============================================================================
# UFPE - Departamento de Ciência Política
# ARQUIVO: percentual_votos_secao.R
# OBJETIVO: em cada seção (governador PE, 1998-2022), calcular os votos
#           válidos e o percentual de cada candidato sobre eles.
# CODIFICAÇÃO: UTF-8 | R >= 4.1
# =============================================================================

# GUIA RÁPIDO DE SINTAXE
# pacote::funcao()  função de um pacote específico
# |>                pipe nativo: x |> f() equivale a f(x)
# %in% / !          "está contido em" / negação
# .by = c(...)      faz a conta separadamente para cada seção, ano e turno
# here::here()      monta o caminho a partir da raiz do projeto (.Rproj)


# =============================================================================
# PASSO 0: PACOTES
# =============================================================================
if (!base::requireNamespace("pacman", 
                            quietly = TRUE)) {
  utils::install.packages("pacman")
}

pacman::p_load(
  here,
  tidyverse
)


# =============================================================================
# PASSO 1: LER O BANCO
# =============================================================================
base::message("PASSO 1: lendo o banco")

dados_secao <- base::readRDS(
  here::here("dados", "votacao_secao_PE_1998_2022_governador.rds")
)

dplyr::glimpse(dados_secao)


# =============================================================================
# PASSO 2: VOTOS VÁLIDOS E PERCENTUAL POR SEÇÃO
# =============================================================================
# Votos válidos = tudo menos 95 (branco), 96 (nulo) e 97/98 (anulados).
# PCT_VOTOS_VALIDOS = votos do candidato / votos válidos da seção * 100.
# Brancos, nulos e seções sem voto válido ficam com NA no percentual.
# -----------------------------------------------------------------------------
base::message("PASSO 2: votos válidos e percentual")

dados_secao <- dados_secao |>
  dplyr::mutate(
    VOTO_VALIDO   = !NR_VOTAVEL %in% c(95, 96, 97, 98),
    VOTOS_VALIDOS = base::sum(QT_VOTOS[VOTO_VALIDO]),
    .by = c(ANO_ELEICAO, NR_TURNO, ID_SECAO)
  ) |>
  dplyr::mutate(
    PCT_VOTOS_VALIDOS = dplyr::if_else(
      VOTO_VALIDO & VOTOS_VALIDOS > 0,
      100 * QT_VOTOS / VOTOS_VALIDOS,
      NA_real_
    )
  )

dplyr::glimpse(dados_secao)

# =============================================================================
# PASSO 3: PERCENTUAL DE VOTOS VÁLIDOS AO QUADRADO
# =============================================================================
base::message("PASSO n: elevando PCT_VOTOS_VALIDOS ao quadrado")

dados_secao <- dados_secao |>
  dplyr::mutate(PCT_VOTOS_SQR = PCT_VOTOS_VALIDOS^2)

dplyr::glimpse(dados_secao)

# Inspeção: o quadrado de uma proporção (0 a 1) nunca é maior que ela
dados_secao |>
  dplyr::select(PCT_VOTOS_VALIDOS, PCT_VOTOS_SQR) |>
  base::summary()


# =============================================================================
# PASSO FINAL: REPRODUTIBILIDADE
# =============================================================================
utils::sessionInfo()