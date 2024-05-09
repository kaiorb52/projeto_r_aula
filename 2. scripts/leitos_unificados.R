
pacman::p_load(
  tidyverse,
  stringi
)

abrir_leitos <- function(ano) {
  setNames(lapply(ano, function(i) {
    df <- read.csv(paste0("0. data-raw/Leitos_", i, ".csv"))
    df %>%
      mutate(NU_ANO = {{i}})
  }),
  paste0("f_", ano))
}

l_unificado_rj <- abrir_leitos(ano = 2016:2022) |>
  bind_rows() |>
  filter(UF=="RJ")

hospital_rj <- l_unificado_rj |>
  group_by(MUNICIPIO, NOME.ESTABELECIMENTO, TP_GESTAO, DS_TIPO_UNIDADE, DESC_NATUREZA_JURIDICA) |>
  summarise(max_leitos = max(LEITOS.EXISTENTE), max_uti = max(UTI.TOTAL...EXIST)) |>
  ungroup()

mun_rj_leitos <- hospital_rj |>
  group_by(MUNICIPIO) |> 
  summarise(contagem_hospitais = n(), 
            sum_leitos = sum(max_leitos), 
            sum_uti = sum(max_uti))

saveRDS(mun_rj_leitos, "1. data/mun_rj_leitos.rds")




