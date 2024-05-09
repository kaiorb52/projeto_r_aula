
abrir_finbra <- function(ano) {
  setNames(lapply(ano, function(i) {
    df <- readRDS(paste0("0. data-raw/finbra/finbra_", i, ".rds"))
    if ("Total Receitas" %in% colnames(df)) {variaveis <- c("UF", "Instituição", "Cod.IBGE", "População", "Total Receitas", "Total Geral da Despesa")
    } else {variaveis <- c("UF", "Instituição", "Cod.IBGE", "População", "TOTAL DAS RECEITAS (III) = (I + II)", "Total Geral da Despesa")
    }
    df %>%
      select(variaveis,
             starts_with("10")) %>%
      mutate(NU_ANO = {{i}})
  }),
  paste0("f_", ano))
}

abrir_leitos <- function(ano) {
  setNames(lapply(ano, function(i) {
    df <- read.csv(paste0("0. data-raw/Leitos_", i, ".csv"))
    df %>%
      mutate(NU_ANO = {{i}})
  }),
  paste0("f_", ano))
}

abrir_df <- function(ano) {
  df <- read.csv(paste0("0. data-raw/Leitos_", ano, ".csv"))
  df
}
