
# SCRIPT Final da Disciplina R
# Nome: Kaio R. Barbara

# ===========================================================================
# 
# Este script é uma sistematização inicial com os dados da produção de barril de 
# petroleo da Agencia Nacional do Petroleo, presentes no caminho: "0. data-raw/",
# encontrado na url: "".
# Que foram baixados e extraidos com o script python "2. scripts/Baixador_anp_pocos_V2.py"
# e reordernados a mão pelo autor.
# Ademais, foi utilizado tambem do banco de dados xxx tambem da ANP,
# encontrado na url: "". Esse fonte de dados foi utilizada para aglomerar 
# informações relativas a lat. e long. dos poços e outros detalhes. 
# 
# ===========================================================================

# Objetivo da pesquisa:

# A presente exploração tem o intuito de levantar fatores que diferenciam as
# bacias de Santos e de Campos. Analisando a evolução de ambas as bacias produtoras 
# e buscando diagnosticar tendências. A hipótese do trabalho seria se existiria 
# uma inerente instabilidade política na produção de petróleo.

options(scipen = 999)
set.seed(100)

pacman::p_load(
  tidyverse,
  readxl,
  glue, 
  lubridate,
  stringi
)

# 0.Funções =================================================================

pacotes_load <- function(...){

  # Em geral essa função existe por pura preguiça de abrir/esquecer os pacotes
  # A ideia era fazer uma função utilizando de poucas funções e sem utilizar o 
  # for loop. Fiquei satisfeito com o resutaldo, mas acho que tem o que melhorar.

  if (!all(c("tidyverse", "glue", "readxl") %in% .packages())){
    pacman::p_load(tidyverse, glue, readxl)  
  } 

}

import_producao1 <- function(ano, mes, skip) {
  
  # Responsavel pela importação dos dados das produção dos poços de petroleo.
  # A função carrega as respectivas panilhas xlsx do meses e anos selecionado.
  # Faz o trabalho de sistematizar a manipulação inicial do banco de dados,
  # transformando os xlsxs importados em uma lista, limpa e descarta variaveis 
  # desnecessarias.
  
  pacotes_load()
  
  setNames(lapply(mes, function(i) {
    
    df <- read_excel(as.character(glue("0. data-raw/producao-pocos-{ano}/{ano}_{i}_producao_mar.xlsx")),
                     col_names = F, skip = skip)|> 
      select(1:14)
    
    cols_names <- c("Estado", "Bacia", "Nome Poço-ANP", "Nome Poço-Operador", "Campo", "Operador", "Número do Contrato", "Período", 
                     "Óleo (bbl/dia)", "Condensado (bbl/dia)", "Petróleo (bbl/dia)", "Gás Natural (Mm³/dia)-Associado", 
                     "Gás Natural (Mm³/dia)-Não Associado", "Gás Natural (Mm³/dia)-Gás Total")
    
    colnames(df)[1:length(cols_names)] <- cols_names
    
    df <- df |>
      mutate(
        #NU_ANO = {{ano}}, NU_MES = {{i}}, 
        `Petróleo (bbl/dia)` = as.numeric(`Petróleo (bbl/dia)`)
      ) |>
      select(-`Óleo (bbl/dia)`, -`Condensado (bbl/dia)`, -`Gás Natural (Mm³/dia)-Associado`, -`Gás Natural (Mm³/dia)-Não Associado`, -`Gás Natural (Mm³/dia)-Gás Total`) |>
      janitor::clean_names()
  }),
  paste0(ano, "_", mes)) |>
    bind_rows()
}

import_producao2 <- function(path) {

  # A presente função é outra facilitadora para a importação das panilhas da produção 
  # por poços. Contudo é uma elaboração mais simples, se comparado a anterior. 
  # Ela foi feita para os anos que os dados, eram apenas armazenados em uma unica panilha 
  # anual, como foi no caso de 2005-2018.

  pacotes_load()
  
  cols_names <- c("Estado", "Bacia", "Nome Poço-ANP", "Nome Poço-Operador", "Campo", "Operador", "Número do Contrato", "Período", 
                   "Óleo (bbl/dia)", "Condensado (bbl/dia)", "Petróleo (bbl/dia)", "Gás Natural (Mm³/dia)-Associado", 
                   "Gás Natural (Mm³/dia)-Não Associado", "Gás Natural (Mm³/dia)-Gás Total")
  
  df <- read_excel(path, col_names = FALSE, skip = 7) |>
    select(1:14)
  
  colnames(df) <- cols_names[1:14]
  
  df <- df |>
    select(-`Óleo (bbl/dia)`, -`Condensado (bbl/dia)`, 
           -`Gás Natural (Mm³/dia)-Associado`, 
           -`Gás Natural (Mm³/dia)-Não Associado`, 
           -`Gás Natural (Mm³/dia)-Gás Total`,
           -`Número do Contrato`) |>
    janitor::clean_names()
  
  return(df)
}

themes_pallet <- function(pallet = F){
  
  # Essa função esta aqui por conviniencia e para facilitar o trabalho de ajustar,
  # os graficos para as preferencias do autor.
  
  cores <- function(...){
    scale_color_manual(
      values = c(
        MetBrewer::met.brewer("Homer2", 3)
      )
    )
  }
  
  p <- ggthemes::theme_clean() +
  theme(
    legend.position = "bottom",
    legend.background = element_rect(colour = "white"),
    axis.text.y = element_text(size = 12, face = "italic"),
    axis.text.x = element_text(size = 14, face = "bold"),
    plot.background = element_rect(colour = "white")
  )
  
  if (pallet == TRUE) {
    return(list(p, cores()))
  } else {
    return(list(p))
  }
}

save(themes_pallet, file = "1. data/theme_pallet")

# 1. Criação dos data.frames ================================================
## 1.1 Produção dos poços de petroleo unificados ============================

  # Esse sistematização cria o principal data.frame do projeto. O segmento utiliza
  # da função import_producao1 e 2 para abrir as panilhas xlsx. É utilizado de listas 
  # para armazenar esses dfs e com isso é utilizado do bind_rows para unir toda a 
  # lista. 

  # O produto final é o df producao_u(nificado) que compoe a produção de petroleo 
  # de cada poço de exploração em determinado mes do ano. Os anos disponiveis pela anp
  # são de 2005-2023 e o df é composto por 10 colunas (variaveis) e 225.543 linhas.

list_prod <- list(
    prod_2023 = import_producao1(ano = 2023, sprintf("%02d", 1:9), skip = 3),
    prod_2022 = import_producao1(ano = 2022, sprintf("%02d", 1:12), skip = 3),
    prod_2021 = import_producao1(ano = 2021, sprintf("%02d", 1:12), skip = 3),
    prod_2020 = import_producao1(ano = 2020, sprintf("%02d", 1:12), skip = 3),
    prod_2019 = import_producao1(ano = 2019, sprintf("%02d", 1:12), skip = 3)
  )

for (ano in 2005:2018){
  library(glue)
  df_name <- glue("prod_{ano}")
  list_prod[[df_name]] <- import_producao2(path = as.character(glue("0. data-raw/producao-pocos-{ano}/{ano}_producao_mar.xlsx")))
}

producao_u <- list_prod |>
  janitor::clean_names() |>
  bind_rows() |>
  mutate(recorte=case_when(
    bacia =="Campos"~bacia,
    bacia =="Santos"~bacia,
    TRUE ~ "Outros"
  )) |>
  janitor::clean_names() |>
  filter(
    !is.na(estado) &
    operador != "Operador" & 
    bacia != "Bacia"
  ) 

saveRDS(file = "1. data/dados_tratados/producao_u.rds", object = producao_u)

rm(list = ls())

## 1.2 df das informações poços Maritimos ===================================

  # O data.frame criado no codigo a seguir é uma importação da tabelas de poços,
  # que contem variaveis com detalhes dos poços como profundidade da perfuração,
  # variaveis categoricas para se o poço é do pre-sal ou não, se é um poço maritimo
  # ou em terra e outros detalhes. 
  
  # Dessa forma, o objeto criado:pocos_m_2024, possue 7075 linhas e 76 colunas. 
  # Sendo, cada linha respectivo a um poço de petroleo. O que constata que ao menos 
  # do que se tem consetimento ou foi publicado pela ANP, foram encontrados 
  # 7075 poços no Brasil.

pocos_m_2024 <- data.table::fread("0. data-raw/Tabela_pocos_2024_Maio_12.csv", encoding = "Latin-1") |> 
  filter(TERRA_MAR=="M") |>
  mutate(
    data_conclusao=lubridate::dmy(CONCLUSAO),
    Ano_inicio=lubridate::year(dmy(INICIO)),
    Ano_termino=lubridate::year(dmy(TERMINO)),
    Ano_conclusao=lubridate::year(dmy(CONCLUSAO)),
    LATITUDE_BASE_DD = as.character(LATITUDE_BASE_DD),
    LONGITUDE_BASE_DD = as.character(LONGITUDE_BASE_DD),
    LATITUDE_BASE_DD = stri_replace_all_fixed(LATITUDE_BASE_DD, ",", "."),
    LONGITUDE_BASE_DD = stri_replace_all_fixed(LONGITUDE_BASE_DD, ",", ".")
  ) |>
  mutate(
    presal = case_when(
      ATINGIU_PRESAL == "S" ~ "Poços do pre-sal",
      TRUE ~ "Outros"),
    presal = factor(presal, levels = c("Poços do pre-sal", "Outros")),
    recorte = case_when(
      BACIA == "Campos" ~ BACIA,
      BACIA == "Santos" ~ BACIA,
      TRUE ~ "Outros"
    )
  ) |>
  janitor::clean_names()

saveRDS(file = "1. data/dados_tratados/pocos_m_2024.rds", object = pocos_m_2024)

rm(list = ls())

# 1.3 Mesclagem dos dfs producao_u & pocos_m_2024 ===========================

  # O codigo ultiliza dos dois df criados anteriormente para criar um terceiro 
  # mais robusto. Nele costara as informações de ambos as fontes, o que possibita 
  # a correlação de variaveis da natureza dos poços. 

pocos_m_2024 <- readRDS("1. data/dados_tratados/pocos_m_2024.rds")

producao_u <- readRDS("1. data/dados_tratados/producao_u.rds")

pocos_prod <- producao_u |>
  filter(petroleo_bbl_dia > 1) |>
  left_join(pocos_m_2024, by= c("nome_poco_anp"="poco","recorte")) |>
  mutate(
    ano_mes = ym(paste0(periodo)),
    profundidade_numeric = as.numeric(stri_replace_all_fixed(profundidade_vertical_m, ",", "."))
  )

  # O banco poco_prod contem 76 colunas e 174.366 linhas. A redução no numero de 
  # linhas se comparado ao producao_u(nificado), ocorre devido a filtragem feita 
  # anteriormente dos poços que não tiveram produção de petroleo. A existencia 
  # dessas casos ocorre, pois nem todos os poços são produtores de petroleo, uma 
  # gama deles possuem apenas gas natural.

saveRDS(file = "1. data/dados_tratados/pocos_prod.rds", object = pocos_prod)

rm(list = ls())

# 2. Graficos ===============================================================
# 2.0 Importação dos dfs necessarios ========================================

pocos_prod <- readRDS("1. data/dados_tratados/pocos_prod.rds")
save(pocos_prod, file = "1. data/dados_tratados/pocos_prod.rda")
load("1. data/theme_pallet")

## 2.1 Graficos Exploratorios com os dados dos poços ========================

