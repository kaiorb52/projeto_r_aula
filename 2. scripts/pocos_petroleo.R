
# SCRIPT Final da Disciplina R
# Nome: Kaio R. Barbara

# ===========================================================================
#
# Este script é uma sistematização inicial com os dados da Producao de barril de
# petroleo da Agencia Nacional do Petroleo, presentes no caminho:
# "https://github.com/kaiorb52/projeto_r_aula/tree/main/0.%20data-raw". Que
# foram baixados e extraidos com o script python:
# "https://github.com/kaiorb52/projeto_r_aula/blob/main/2.%20scripts/Baixador_anp_pocos_V2.py"
# e reordernados manualmente pelo autor. Ademais, foi utilizado tambem do banco
# de dados "Tabela_pocos_2024_Maio_12" tambem da ANP. Esse fonte de dados foi
# utilizada para aglomerar informações relativas a lat. e long. dos pocos e
# outros detalhes.
#
# ===========================================================================

# Objetivo da pesquisa:

# A presente exploração tem o intuito de levantar fatores que diferenciam as
# bacias de Santos e de Campos. Analisando a evolução de ambas as bacias
# produtoras e buscando diagnosticar tendências. A hipótese do trabalho seria se
# existiria uma inerente instabilidade política na Producao de petróleo.

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

  # Em geral essa função existe por pura preguiça de abrir/esquecer os pacotes A
  # ideia era fazer uma função utilizando de poucas funções e sem utilizar o for
  # loop. Fiquei satisfeito com o resutaldo, mas acho que tem o que melhorar.

  if (!all(c("tidyverse", "glue", "readxl") %in% .packages())){
    pacman::p_load(tidyverse, glue, readxl)  
  } 

}

import_producao1 <- function(ano, mes, skip) {
  
  # Responsavel pela importação dos dados das Producao dos pocos de petroleo. A
  # função carrega as respectivas panilhas xlsx do meses e anos selecionado. Faz
  # o trabalho de sistematizar a manipulação inicial do banco de dados,
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

  # A presente função é outra facilitadora para a importação das panilhas da
  # Producao por pocos. Contudo é uma elaboração mais simples, se comparado a
  # anterior. Ela foi feita para os anos que os dados, eram apenas armazenados
  # em uma unica panilha anual, como foi no caso de 2005-2018.

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
  
  cores <- function(...) {
    scale_color_manual(values = c(MetBrewer::met.brewer("Homer2", 3)))
  }

  p <- ggthemes::theme_clean() +
  theme(
    legend.position = "bottom",
    legend.background = element_rect(colour = "white"),
    axis.text.y = element_text(size = 14, face = "italic"),
    axis.text.x = element_text(size = 16, face = "bold"),
    strip.text = element_text(size = 15, face = "italic"),
    plot.background = element_rect(colour = "white")
  )
  
  if (pallet == TRUE) {
    return(list(p, cores()))
  } else {
    return(list(p))
  }
}

funcoes <- list(
    pacotes_load = pacotes_load, 
    import_producao1 = import_producao1,
    import_producao2 = import_producao2,
    themes_pallet = themes_pallet
  ) 
  
save(funcoes, file = "1. data/funcoes") +
rm(funcoes)

# 1. Criação dos data.frames ================================================
## 1.1 Producao dos pocos de petroleo unificados ============================

  # Esse sistematização cria o principal data.frame do projeto. O segmento
  # utiliza da função import_producao1 e 2 para abrir as panilhas xlsx. É
  # utilizado de listas para armazenar esses dfs e com isso é utilizado do
  # bind_rows para unir toda a lista.

  # O produto final é o df producao_u(nificado) que compoe a Producao de
  # petroleo de cada poço de exploração em determinado mes do ano. Os anos
  # disponiveis pela anp são de 2005-2023 e o df é composto por 10 colunas
  # (variaveis) e 225.543 linhas.

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
  mutate(
  recorte = case_when(
    bacia == "Campos" ~ paste0(bacia, "/RJ"),
    bacia == "Santos" ~ paste0(bacia, "/RJ"),
    TRUE ~ "Outras Bacias")
  ) |>
  filter(
    !is.na(estado) &
    operador != "Operador" & 
    bacia != "Bacia"
  ) 

saveRDS(file = "1. data/dados_tratados/producao_u.rds", object = producao_u)

rm(list = ls())

## 1.2 df das informações pocos Maritimos ===================================

  # O data.frame criado no codigo a seguir é uma importação da tabelas de pocos,
  # que contem variaveis com detalhes dos pocos como profundidade da perfuração,
  # variaveis categoricas para se o poço é do pre-sal ou não, se é um poço maritimo
  # ou em terra e outros detalhes. 
  
  # Dessa forma, o objeto criado:pocos_m_2024, possue 7075 linhas e 76 colunas. 
  # Sendo, cada linha respectivo a um poço de petroleo. O que constata que ao menos 
  # do que se tem consetimento ou foi publicado pela ANP, foram encontrados 
  # 7075 pocos no Brasil.

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
  janitor::clean_names() |>
  mutate(
    presal = case_when(
      atingiu_presal == "S" ~ "pocos do pre-sal",
      TRUE ~ "Outros"
    ),
    presal = factor(presal, levels = c("pocos do pre-sal", "Outros")),
    recorte = case_when(
      bacia == "Campos" ~ paste0(bacia, "/RJ"),
      bacia == "Santos" ~ paste0(bacia, "/RJ"),
      TRUE ~ "Outras Bacias"
    ),
  )

saveRDS(file = "1. data/dados_tratados/pocos_m_2024.rds", object = pocos_m_2024)

rm(list = ls())

## 1.3 Mesclagem dos dfs producao_u & pocos_m_2024 ===========================

  # Esse sistematização cria o principal data.frame do projeto. O codigo ultiliza 
  # dos dois df criados anteriormente para criar um terceiro mais robusto. 
  # Nele costara as informações de ambos as fontes, o que possibita a correlação 
  # de variaveis da natureza dos pocos com a produlção de barril de petroleo. 

pocos_m_2024 <- readRDS("1. data/dados_tratados/pocos_m_2024.rds") |>
  select(-operador)
  
  # A necessidade de remover a variavel operador, vem que ambos os bancos pocos_m_2024 &
  # producao_u, possuem essa coluna. Contudo a informação entre os dois é conflitante.
  # Foi decidido utilizar a var. do df producao_u, Pelo fato de o producao_u fazer uma 
  # serie temporal, mes a mes, enquanto que o pocos_m_2024 traz informações tabuladas pela 
  # ANP, no periodo mais recente. Assim é mais adequado para fazer o acompanhamento da 
  # evolução dessa var o banco producao_u.

producao_u <- readRDS("1. data/dados_tratados/producao_u.rds") 

pocos_prod <- producao_u |>
  filter(petroleo_bbl_dia > 1) |>
  left_join(pocos_m_2024, by= c("nome_poco_anp"="poco","recorte")) |>
  mutate(
    ano_mes = ym(paste0(periodo)),
    profundidade_numeric = as.numeric(stri_replace_all_fixed(profundidade_vertical_m, ",", ".")),
    petrobras = ifelse(operador == "Petrobras", operador, "Outras Empresas"),
    petrobras = factor(petrobras, levels = c("Petrobras", "Outras Empresas"))
  )

  # O banco poco_prod contem 75 colunas e 174.366 linhas. A redução no numero de 
  # linhas se comparado ao producao_u(nificado), ocorre devido a filtragem feita 
  # anteriormente dos pocos que não tiveram Producao de petroleo. A existencia 
  # dessas casos ocorre, pois nem todos os pocos são produtores de petroleo, uma 
  # gama deles possuem apenas gas natural.

saveRDS(file = "1. data/dados_tratados/pocos_prod.rds", object = pocos_prod)

rm(list = ls())

# 2. Graficos ===============================================================
# 2.0 Importação dos dfs necessarios ========================================
## 2.1 Graficos Exploratorios com os dados dos pocos ========================

pocos_prod <- readRDS("1. data/dados_tratados/pocos_prod.rds")
load("1. data/funcoes")

criar_graph <- function(..., data = pocos_prod, cor = NULL, titulo = NULL, paleta = F, legenda = NULL, facet = FALSE, y_var = petroleo) {

  options(scipen = 999)
  funcoes$pacotes_load()

  p <- data |>
  mutate(periodo = lubridate::ym(periodo)) |>
  group_by(...) |>
  summarise(count = n(), petroleo = sum(petroleo_bbl_dia), .groups = 'drop') |>
  filter(count > 3) |>
  ggplot(aes(x = periodo, y = {{y_var}})) +
  geom_line(size = 2.0, aes(color = {{cor}})) +
  labs(
    x = NULL,
    y = NULL, 
    title = titulo,
    color = legenda
  ) +
  scale_x_date(
    limits = as.Date(c("2005-01-01", "2023-12-31")),
    breaks = seq(as.Date("2005-01-01"), as.Date("2023-12-31"), by = "2 year"),
    date_labels = "%Y"
  ) +
  funcoes$themes_pallet(pallet = paleta) 
  
  if (facet == TRUE){
    p <- p + facet_wrap(.~recorte) 
  }

  return(p)
}

y_ajust <- function(type = 1){
  
  if (type == 1){
    x <- scale_y_continuous(
      breaks = seq(0, 5000000, by = 500000),
      labels = scales::dollar_format(prefix = "", big.mark = ".", decimal.mark = ",")
    )
    return(x)
  }
  if (type == 2){
    x <- scale_y_continuous(
      breaks = seq(0, 1000000, by = 100),
      labels = scales::dollar_format(prefix = "", big.mark = ".", decimal.mark = ",")
    )
    return(x)
  }
}
colors <- function(){scale_color_manual(values = c("limegreen","skyblue3"))}

list_graficos <- list(
  g_1 = criar_graph(periodo, titulo = "Producao de barril de petroleo por dia, Producao Acumulada(2005-2023)") + y_ajust(),
  g_2 = criar_graph(
    periodo,
    recorte,
    titulo = "Producao de barril de petroleo por dia, Producao por Bacia(2005-2023)",
    cor = recorte,
    paleta = T
  ) + y_ajust(),
  g_3 = criar_graph(
    periodo,
    petrobras,
    titulo = "Producao de barril de petroleo por dia, Producao por Operador(2005-2023)",
    cor = petrobras,
    paleta = T
  ) + y_ajust() + colors(),
  g_4 = criar_graph(periodo, titulo = "Quantidade de pocos produzindo petroleo(2005-2023)", y_var = count) + y_ajust(2),
  g_5 = criar_graph(
    periodo,
    recorte,
    titulo = "Quantidade de pocos produzindo petroleo, Quantidade por Bacia(2005-2023)",
    y_var = count,
    cor = recorte,
    paleta = T
  ) + y_ajust(2),
  g_6 = criar_graph(
    periodo,
    petrobras,
    titulo = "Quantidade de pocos produzindo petroleo, Quantidade por Operador(2005-2023)",
    y_var = count,
    cor = petrobras,
    paleta = T
  ) + y_ajust(2) + colors(),
  g_7 = criar_graph(
    periodo,
    recorte,
    petrobras,
    titulo = "Producao de barril de petroleo por dia, Producao por Operador nas Bacias de Santos e Campos/RJ(2005-2023)",
    data = subset(pocos_prod, recorte != "Outras Bacias"),
    cor = petrobras,
    facet = T,
    y_var = petroleo
  ) + y_ajust() + colors(),
  g_8 = criar_graph(
    periodo,
    recorte,
    petrobras,
    titulo = "Quantidade de pocos produzindo petroleo, Quantidade por Operador nas Bacias de Santos e Campos/RJ(2005-2023)",
    data = subset(pocos_prod, recorte != "Outras Bacias"),
    cor = petrobras,
    facet = T,
    y_var = count
  ) + y_ajust(2) + colors(),
  g_9 = criar_graph(periodo, y_var = (petroleo / count), titulo = "Produtividade dos pocos, Produtividade Acumulada(2005-2023)"),
  g_10 = criar_graph(
    periodo,
    recorte,
    cor = recorte,
    y_var = (petroleo / count),
    titulo = "Produtividade dos pocos, Produtividade por bacia (2005-2023)",
    paleta = T
  ),
  g_11 = criar_graph(
    periodo,
    petrobras,
    cor = petrobras,
    y_var = (petroleo / count),
    titulo = "Produtividade dos pocos, Produtividade por operador (2005-2023)"
  ) + colors(),
  g_12 = criar_graph(
    periodo,
    recorte,
    petrobras,
    titulo = "Produtividade dos pocos, por Operador nas Bacias de Santos e Campos/RJ(2005-2023)",
    data = subset(pocos_prod, recorte != "Outras Bacias"),
    cor = petrobras,
    facet = T,
    y_var = (petroleo / count)
  ) + colors()
)

## 2.2 Salvamento dos graficos ==============================================

for (g_name in names(list_graficos)){
  library(glue)
  grafico <- list_graficos[[g_name]]
  ggsave(
    plot = grafico,
    filename = as.character(glue("4. figs/{g_name}.png")),
    dpi = 400,
    height = 7,
    width = 12
  )
}

# Conclusão e analise:

  # Os graficos exploratorios indicam que ao decorrer do recorte temporal
  # analisado, 2005-2023, em geral a produção de barris de petroleo apenas
  # aumentou(g_1). Contudo, esse cresimento teve porpoções desiguiais nas
  # diferentes bacias, como mostra o grafico g_2. A bacia de Campos/RJ teve uma
  # produção estavel e alta de 2005 a 2016, até que em 2017 ela declinou,
  # enquanto a bacia de Santos/RJ teve um crescimento constante, ultrapassando a
  # produção de Campos/RJ em 2018. O g_3, demonstra que em geral a Petrobras se
  # posiciona como o maior explorador de Petroleo brasileiro, enquanto o grupo
  # de "Outras Empresas", representam uma amostra pequena da produção nacional.
  # Esse diagnostico é em geral esperado mediante ao fato que historicamente a
  # Petrobras possuiu monopolio da exploração do petroleo.
  
  # Analisando num outro ambito, a contagem de poços sendo explorados sempre
  # manteve o mesmo nivel, contudo com uma queda significativa em 2020 (g_4). O
  # g_5 traz uma perspectiva panoramica dessa queda. No grafico é possivel
  # identificar que a bacia de Campos/RJ sempre teve o maior numero de poços
  # explorados, mas em consonancia com o grupo "Outras Bacias" houve uma queda
  # de sua contagem em 2020. O g_6 apresenta o dianostico, que essa queda em
  # especial ocorre na Petrobras, enquanto que "Outras Empresas" houve um
  # aumento razoavel. O g_7 e o g_8 mostram a historia completa, é possivel
  # indentificar neles que a produção da petrobras tem caido na bacia de
  # Campos/RJ, enquanto em Santos/RJ aumentou, esse mesmo cenario é resiproca
  # para a quantidade de poços sendo explorados. Pelo que da para observar a
  # Petrobras deslocou sua produção para a bacia de Santos/RJ no ano de 2020.
  # Numa pesquisa raza pode dectar três fatores para essa movimentação: i) A
  # Petrobras hibernou duas plantaformas de petroleo em Campos/RJ, em 2020. ii)
  # A ANP cedeu de 10 campos antigos da Petrobras em Campos/RJ para outras
  # empresas, tambem no mesmo periodo. iii) A atratividade de novos poços de
  # exploração no presal.

  # Em parte pode se observar uma produção estavel das bacias. Contudo com
  # resalvas de desmobilização politica. A exploração e a discusão não comprovam
  # a hipotese da existecia de uma inerente instabilidade politica na produção
  # de petroleo, pelo contrario. A produção de petroleo se demonstrou no recorte
  # analise extremamente estavel, com a evolução e mudanças na produção de
  # petroleo e quantitavivo de poços apresentando um claro padrão no
  # comportamento. Além disso, foi possivel averiguar tambem que as mudanças nas
  # movimentações da petrobras aconteceram por movimentações estrategicas para
  # colocar ou para manter a empresa em um cenario de produtividade. Fato esse
  # que se comprova com os graficos g_11 e g_12.


