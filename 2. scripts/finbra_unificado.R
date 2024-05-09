
library(tidyverse)

  library(stringi)
stringi::stri_replace()
abri_finbra <- function(ano) {
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

f_unificiado <- 
  bind_rows(abri_finbra(ano = 2014:2022)) %>%
  select(NU_ANO, everything()) %>%
  mutate(total_geral_receitas = case_when(
    is.na(`Total Receitas`) ~ `TOTAL DAS RECEITAS (III) = (I + II)`,
    !is.na(`Total Receitas`) ~ `Total Receitas`,
    TRUE ~ `Total Receitas`
  )) %>%
  select( -`Total Receitas`, -`TOTAL DAS RECEITAS (III) = (I + II)`) %>%
  janitor::clean_names()

f_unificiado <- 
  f_unificiado %>% 
  mutate(instituicao = str_replace(instituicao, "Prefeitura Municipal (de|do) ", ""),
    munic = str_replace(instituicao, " - RJ", "")) 

saveRDS(f_unificiado, "1. data/f_unificiado.rds")

rm(list = ls())
