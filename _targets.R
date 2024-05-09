library(targets)
library(tarchetypes)
source("2. scripts/funções.R")
tar_option_set(envir = environment(), packages = c("tidyverse", 
    "stringi"))
list(tar_target(anos, 2017:2022), tar_target(dfs, abrir_leitos(anos)))
