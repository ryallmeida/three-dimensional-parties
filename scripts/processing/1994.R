# =============================================================================
# UFPE - Ciência Política | governador_pe_1994_zona.R
# OBJETIVO: competição para governador de PE em 1994 (1º turno).
#   O TSE não publica 1994 por seção: zona -> tabela; município -> mapa.
# SAÍDAS: saidas/governador_pe_1994_{zona,municipio}.csv
#         saidas/mapa_governador_pe_1994.{png,pdf}
# UTF-8 | R >= 4.1
# =============================================================================

# =============================================================================
# PASSO 0: PACOTES E CONFIGURAÇÃO 
# =============================================================================

pacotes <- c("dplyr", "readr", "stringr", "ggplot2", "sf", "geobr", "here")
utils::install.packages(base::setdiff(pacotes, base::rownames(utils::installed.packages())))
base::options(timeout = 600)
base::dir.create(here::here("dados_brutos"), showWarnings = FALSE)
base::dir.create(here::here("saidas"), showWarnings = FALSE)

url_tse <- "https://cdn.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona/votacao_candidato_munzona_1994.zip"
url_tse_ibge <- "https://raw.githubusercontent.com/betafcc/Municipios-Brasileiros-TSE/master/municipios_brasileiros_tse.csv"


# =============================================================================
# PASSO 1: DOWNLOAD E LEITURA 
# =============================================================================

# LEIAME: latin1, ";", aspas; #NULO/#NE em texto e -1/-3 em números = sem dado.
base::message("PASSO 1: download e leitura")
zip_tse <- here::here("dados_brutos", base::basename(url_tse))
if (!base::file.exists(zip_tse)) utils::download.file(url_tse, zip_tse, mode = "wb", quiet = FALSE)
utils::unzip(zip_tse, exdir = here::here("dados_brutos"))

votos_brutos <- readr::read_delim(
  here::here("dados_brutos", "votacao_candidato_munzona_1994_PE.csv"),
  delim = ";", na = c("", "#NULO", "#NE"),
  locale = readr::locale(encoding = "latin1"),
  col_types = readr::cols(.default = readr::col_character()))
dplyr::glimpse(votos_brutos)

# =============================================================================
# PASSO 2: GOVERNADOR, 1º TURNO 
# =============================================================================

# Usa votos VÁLIDOS; se vierem #NE em 1994, cai para os nominais.
base::message("PASSO 2: filtrar governador")
votos <- votos_brutos |>
  dplyr::filter(stringr::str_to_upper(DS_CARGO) == "GOVERNADOR", NR_TURNO == "1") |>
  dplyr::mutate(
    dplyr::across(c(QT_VOTOS_NOMINAIS_VALIDOS, QT_VOTOS_NOMINAIS),
                  \(x) base::replace(base::as.numeric(x), 
                                     base::as.numeric(x) %in% c(-1, -3), NA)),
    votos = dplyr::coalesce(QT_VOTOS_NOMINAIS_VALIDOS, 
                            QT_VOTOS_NOMINAIS),
    candidato = base::paste0(NM_URNA_CANDIDATO, 
                             " (", SG_PARTIDO, ")"))

dplyr::glimpse(votos)

dplyr::count(votos, candidato, wt = votos, sort = TRUE)

base::stopifnot(base::nrow(votos) > 0,
                !base::anyNA(votos$votos),
                base::all(stringr::str_detect(votos$CD_MUNICIPIO, 
                                              "^[0-9]+$")))

# =============================================================================
# PASSO 3: INDICADORES (zona e município)
# =============================================================================

# margem = 1º - 2º (p.p.); nec = 1 / soma(p²), número efetivo de candidatos.
base::message("PASSO 3: indicadores")
competicao <- function(dados, ...) {
  dados |>
    dplyr::summarise(votos = base::sum(votos), .by = c(..., candidato)) |>
    dplyr::mutate(p = votos / base::sum(votos), .by = c(...)) |>
    dplyr::arrange(dplyr::desc(p)) |>
    dplyr::summarise(vencedor = dplyr::first(candidato),
                     margem   = 100 * (p[1] - dplyr::coalesce(p[2], 0)),
                     nec      = 1 / base::sum(p^2),
                     .by = c(...))
}

por_zona <- competicao(votos, NR_ZONA, CD_MUNICIPIO, NM_MUNICIPIO)
por_municipio <- competicao(votos, CD_MUNICIPIO, NM_MUNICIPIO)
dplyr::glimpse(por_municipio)

# =============================================================================
# PASSO 4: CÓDIGO TSE -> IBGE E MALHA 
# =============================================================================
# geobr não tem 1994: malha 2000; municípios criados depois ficam em cinza.

base::message("PASSO 4: junção espacial")
tse_ibge <- readr::read_csv(url_tse_ibge, col_types = readr::cols(.default = "c")) |>
  dplyr::filter(uf == "PE") |>
  dplyr::transmute(CD_MUNICIPIO = stringr::str_pad(codigo_tse, 5, pad = "0"),
                   code_muni = base::as.numeric(codigo_ibge))

por_municipio <- por_municipio |>
  dplyr::mutate(CD_MUNICIPIO = stringr::str_pad(CD_MUNICIPIO, 5, pad = "0")) |>
  dplyr::left_join(tse_ibge, by = "CD_MUNICIPIO")

malha <- geobr::read_municipality("PE", year = 2000)
sem_par <- por_municipio |> dplyr::filter(base::is.na(code_muni) | !code_muni %in% malha$code_muni)
if (base::nrow(sem_par) > 0) base::stop("Sem código IBGE/polígono: ", base::toString(sem_par$NM_MUNICIPIO))

mapa <- malha |> dplyr::left_join(por_municipio, by = "code_muni")
dplyr::glimpse(mapa)

# =============================================================================
# PASSO 5: MAPA E EXPORTAÇÃO 
# =============================================================================

base::message("PASSO 5: mapa e exportação")

mapa_sem_noronha <- mapa |>
  dplyr::filter(code_muni != 2605459)   # 2605459 = Fernando de Noronha (IBGE)
dplyr::glimpse(mapa_sem_noronha)

figura <- ggplot2::ggplot(mapa_sem_noronha) +
  ggplot2::geom_sf(ggplot2::aes(fill = margem), colour = "white", linewidth = 0.1) +
  ggplot2::scale_fill_viridis_c(direction = -1, na.value = "grey85", name = "Margem 1º-2º (p.p.)") +
  ggplot2::theme_void() +
  ggplot2::theme(
    legend.position   = "bottom",
    plot.background   = ggplot2::element_rect(fill = "transparent", colour = NA),
    panel.background  = ggplot2::element_rect(fill = "transparent", colour = NA),
    legend.background = ggplot2::element_rect(fill = "transparent", colour = NA)
  ) +
  ggplot2::labs(title = "", subtitle = "", caption = "")

print(figura)

ggplot2::ggsave(here::here("saidas", "margem_governador_pe_1994.png"), figura,
                width = 10, height = 5, dpi = 300, bg = "transparent")

#ggplot2::ggsave(here::here("saidas", "mapa_governador_pe_1994.pdf"), figura, width = 10, height = 5, device = grDevices::cairo_pdf)


#readr::write_csv(por_zona, here::here("saidas", "governador_pe_1994_zona.csv"))
#readr::write_csv(sf::st_drop_geometry(mapa), here::here("saidas", "governador_pe_1994_municipio.csv"))

# =============================================================================
# PASSO 6: TABELA
# =============================================================================

base::message("PASSO 6: tabela de fatias, HHI e NEC (PE, 1º turno)")
# Uma linha por candidato; a linha "Total" traz o HHI (soma dos p²) e o NEC (1/HHI).
tabela_candidatos <- votos |>
  dplyr::summarise(Votos = base::sum(votos), .by = c(NM_URNA_CANDIDATO, SG_PARTIDO)) |>
  dplyr::transmute(Candidato = NM_URNA_CANDIDATO,
                   Sigla     = SG_PARTIDO,
                   Votos,
                   `%`       = 100 * Votos / base::sum(Votos),
                   `p²`      = (Votos / base::sum(Votos))^2) |>
  dplyr::arrange(dplyr::desc(Votos))

tabela_hhi3 <- tabela_candidatos |>
  dplyr::mutate(HHI = base::sum(`p²`),
                Nec = 1 / HHI)

dplyr::glimpse(tabela_hhi3)
print(tabela_hhi3)

utils::sessionInfo()

# =============================================================================
# PASSO 7: p² POR PARTIDO E MUNICÍPIO 
# =============================================================================
# Fatia de cada partido no município (votos válidos), elevada ao quadrado.
# tidyr::complete() cria a linha com 0 voto quando o partido não teve voto ali.

base::message("PASSO 7: p² por partido e município")
fatias_municipio <- votos |>
  dplyr::summarise(votos = base::sum(votos), .by = c(CD_MUNICIPIO, SG_PARTIDO)) |>
  tidyr::complete(CD_MUNICIPIO, SG_PARTIDO, fill = list(votos = 0)) |>
  dplyr::mutate(p = votos / base::sum(votos), p2 = p^2, .by = CD_MUNICIPIO) |>
  dplyr::mutate(CD_MUNICIPIO = stringr::str_pad(CD_MUNICIPIO, 5, pad = "0")) |>
  dplyr::left_join(tse_ibge, by = "CD_MUNICIPIO")
dplyr::glimpse(fatias_municipio)

# Conferência: as fatias de cada município somam 1; nenhum código IBGE faltando.
base::stopifnot(
  base::all(base::abs(dplyr::summarise(fatias_municipio, s = base::sum(p), .by = CD_MUNICIPIO)$s - 1) < 1e-9),
  !base::anyNA(fatias_municipio$code_muni))

malha_sem_noronha <- malha |> dplyr::filter(code_muni != 2605459)


# =============================================================================
# ASSO 8: MAPAS EM PARES (2 linhas × 1 coluna) 
# =============================================================================
# Função: um mapa por partido, com a escala indo do mínimo ao máximo DAQUELE partido.

base::message("PASSO 8: mapas em pares")
mapa_partido <- function(sigla) {
  dados <- malha_sem_noronha |>
    dplyr::left_join(dplyr::filter(fatias_municipio, SG_PARTIDO == sigla), by = "code_muni")
  ggplot2::ggplot(dados) +
    ggplot2::geom_sf(ggplot2::aes(fill = p2), colour = "white", linewidth = 0.1) +
    ggplot2::scale_fill_viridis_c(direction = -1, na.value = "grey85", name = "p²",
                                  limits = base::range(dados$p2, na.rm = TRUE)) +
    ggplot2::labs(title = sigla) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position   = "right",
      plot.background   = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.background  = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA))
}

pares <- list(c("PSB", "PFL"), c("PMDB", "PSTU"), c("PTB", "PRN"))
  
  for (par in pares) {
    figura_par <- mapa_partido(par[1]) / mapa_partido(par[2])   # "/" = um embaixo do outro
    print(figura_par)
    ggplot2::ggsave(here::here("saidas", base::paste0("p2_", par[1], "_", par[2], "_1994.png")),
                    figura_par, width = 8, height = 7, dpi = 300, bg = "transparent")
  }

# ==============================================================================
# PASSO 9: DIAGNÓSTICO, MUNICÍPIOS SEM DADO (CINZA)
# Quem está na malha 2000 mas ficou com margem NA (criado após 1994).

base::message("PASSO 9: municípios ausentes")
mapa_sem_noronha <- mapa |> dplyr::filter(code_muni != 2605459)

ausentes <- mapa_sem_noronha |>
  dplyr::filter(base::is.na(margem)) |>
  dplyr::select(code_muni, name_muni)
print(sf::st_drop_geometry(ausentes))      # a lista para você conferir
base::message("Municípios sem dado: ", base::nrow(ausentes))

figura <- ggplot2::ggplot(mapa_sem_noronha) +
  ggplot2::geom_sf(ggplot2::aes(fill = margem), colour = "white", linewidth = 0.1) +
  ggrepel::geom_label_repel(
    data = ausentes,
    ggplot2::aes(label = name_muni, geometry = geom),
    stat = "sf_coordinates",                     # pega um ponto dentro de cada polígono
    fun.geometry = sf::st_point_on_surface,
    size = 2.5, min.segment.length = 0,          # sempre desenha a seta
    box.padding = 0.6, max.overlaps = Inf,
    arrow = grid::arrow(length = grid::unit(0.015, "npc")),
    fill = "white", label.size = 0.2) +
  ggplot2::scale_fill_viridis_c(direction = -1, na.value = "grey85",
                                name = "Diferença entre 1º e 2º colocados (p.p.)") +
  ggplot2::theme_void() +
  ggplot2::theme(
    legend.position   = "bottom",
    plot.background   = ggplot2::element_rect(fill = "transparent", colour = NA),
    panel.background  = ggplot2::element_rect(fill = "transparent", colour = NA),
    legend.background = ggplot2::element_rect(fill = "transparent", colour = NA)) +
  ggplot2::labs(title = "", subtitle = "", caption = "")

print(figura)

ggplot2::ggsave(here::here("saidas", "margem_governador_pe_1994(1).png"), figura,
                width = 10, height = 5, dpi = 300, bg = "transparent")

# ==============================================================================
# PASSO 10: ONDE O PARTIDO SE FAZ PRESENTE (POSIÇÃO NA PRÓPRIA ESCALA)
# ==============================================================================

base::message("PASSO 10: presença relativa por partido")
limiar_escala <- 0.5   # metade mais escura da escala de cada partido
maximo_setas  <- 12    # acima disso, rotula só os mais escuros

fatias_municipio <- fatias_municipio |>
  dplyr::mutate(posicao_escala = (p2 - base::min(p2)) / (base::max(p2) - base::min(p2)),
                .by = SG_PARTIDO) |>
  dplyr::mutate(presenca = posicao_escala >= limiar_escala,
                ordem = base::rank(-p2, ties.method = "first"),
                rotular = presenca & ordem <= maximo_setas,
                .by = SG_PARTIDO)

diagnostico <- fatias_municipio |>
  dplyr::summarise(municipios_presenca = base::sum(presenca),
                   rotulados = base::sum(rotular), .by = SG_PARTIDO)
print(diagnostico)

presenca_tabela <- fatias_municipio |>
  dplyr::filter(presenca) |>
  dplyr::left_join(sf::st_drop_geometry(dplyr::select(malha, code_muni, name_muni)), by = "code_muni") |>
  dplyr::transmute(Sigla = SG_PARTIDO, Municipio = name_muni,
                   `%` = 100 * p, `p²` = p2, `Posição na escala` = posicao_escala) |>
  dplyr::arrange(Sigla, dplyr::desc(`p²`))
dplyr::glimpse(presenca_tabela)
readr::write_csv(presenca_tabela, here::here("saidas", "presenca_partidos_1994.csv"))

# PASSO 10: MAPAS EM PARES COM SETAS ------------------------------------------
base::message("PASSO 10: mapas em pares")
mapa_partido <- function(sigla) {
  dados <- malha_sem_noronha |>
    dplyr::left_join(dplyr::filter(fatias_municipio, SG_PARTIDO == sigla), by = "code_muni")
  contagem <- dplyr::filter(diagnostico, SG_PARTIDO == sigla)
  
  ggplot2::ggplot(dados) +
    ggplot2::geom_sf(ggplot2::aes(fill = p2), colour = "white", linewidth = 0.1) +
    ggrepel::geom_label_repel(
      data = dplyr::filter(dados, rotular),
      ggplot2::aes(label = name_muni, geometry = geom),
      stat = "sf_coordinates", fun.geometry = sf::st_point_on_surface,
      size = 2, min.segment.length = 0, box.padding = 0.5, max.overlaps = Inf,
      arrow = grid::arrow(length = grid::unit(0.01, "npc")),
      fill = "white", label.size = 0.1) +
    ggplot2::scale_fill_viridis_c(direction = -1, na.value = "grey85", name = "p²",
                                  limits = base::range(dados$p2, na.rm = TRUE)) +
    ggplot2::labs(title = sigla) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position   = "right",
      plot.background   = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.background  = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA))
}

for (par in pares) {
  figura_par <- patchwork::wrap_plots(mapa_partido(par[1]), mapa_partido(par[2]), ncol = 1)
  print(figura_par)
  ggplot2::ggsave(here::here("saidas", base::paste0("p2_", par[1], "_", par[2], "_presenca_1994.png")),
                  figura_par, width = 10, height = 10, dpi = 300, bg = "transparent")
}