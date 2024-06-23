
# SCRIPT Final da Disciplina R
# Nome: Kaio R. Barbara

# ===========================================================================
# 
# Este script é uma sistematização inicial com os dados da produção de barril de 
# petroleo da Agencia Nacional do Petroleo, presentes no caminho: "0. data-raw/",
# encontrado na url: "".
# Que foram baixados e extraidos com o script python "2. scripts/Baixador_anp_pocos_V2.py" 
# e reordernados a mão pelo autor.
# Ademais, foi utilizado do banco de dados xxx tambem da ANP,
# encontrado na url: ""
# Esse fonte de dados foi utilizada para aglomerar informações relativas 
# a lat. e long. dos poços e outros detalhes.
# 
# ===========================================================================

pacman::p_load(
  tidyverse, 
  readxl,
  glue, 
  lubridate,
  stringi)  

options(scipen = 999)

# 0.Funções =================================================================

pacotes_load <- function(...){

  # search()

  if (!all(c("tidyverse", "glue", "readxl") %in% .packages())){
    pacman::p_load(tidyverse, glue, readxl)  
  } 

}

import_producao1 <- function(ano, mes, skip) {
  
  pacotes_load()
  
  setNames(lapply(mes, function(i) {
    
    df <- read_excel(as.character(glue("0. data-raw/producao-pocos-{ano}/{ano}_{i}_producao_mar.xlsx")),
                     col_names = F, skip = skip)|> 
      select(1:14)
    
    novos_nomes <- c("Estado", "Bacia", "Nome Poço-ANP", "Nome Poço-Operador", "Campo", "Operador", "Número do Contrato", "Período", 
                     "Óleo (bbl/dia)", "Condensado (bbl/dia)", "Petróleo (bbl/dia)", "Gás Natural (Mm³/dia)-Associado", 
                     "Gás Natural (Mm³/dia)-Não Associado", "Gás Natural (Mm³/dia)-Gás Total")
    
    colnames(df)[1:length(novos_nomes)] <- novos_nomes
    
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
  
  pacotes_load()
  
  novos_nomes <- c("Estado", "Bacia", "Nome Poço-ANP", "Nome Poço-Operador", "Campo", "Operador", "Número do Contrato", "Período", 
                   "Óleo (bbl/dia)", "Condensado (bbl/dia)", "Petróleo (bbl/dia)", "Gás Natural (Mm³/dia)-Associado", 
                   "Gás Natural (Mm³/dia)-Não Associado", "Gás Natural (Mm³/dia)-Gás Total")
  
  df <- read_excel(path, col_names = FALSE, skip = 7) |>
    select(1:14)
  
  colnames(df) <- novos_nomes[1:14]
  
  df <- df |>
    select(-`Óleo (bbl/dia)`, -`Condensado (bbl/dia)`, 
           -`Gás Natural (Mm³/dia)-Associado`, 
           -`Gás Natural (Mm³/dia)-Não Associado`, 
           -`Gás Natural (Mm³/dia)-Gás Total`,
           -`Número do Contrato`) |>
    janitor::clean_names()
  
  return(df)
}

themes_plot <- function(...){
  ggthemes::theme_clean() +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 12, face = "italic"),
    axis.text.x = element_text(size = 14, face = "bold"),
    plot.background = element_rect(colour = "white")
  )
  
}

pallet <- function(...){
  
  scale_color_manual(
    values = c(
      MetBrewer::met.brewer("Homer2", 3)
    )
  )
  
}

# 1.1 Criação do df da produção dos poços de petroleo unificados ==========================

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
  list_prod[[df_name]] <- import_producao2(path = as.character(glue("0. data-raw/producao-pocos-{ano}/{ano}_producao_mar.xlsx"))) |>
    janitor::clean_names()
}

producao_u <- list_prod |>
  bind_rows() |>
  mutate(recorte=case_when(
    bacia =="Campos"~bacia,
    bacia =="Santos"~bacia,
    TRUE ~ "Outros"
  )) |>
  janitor::clean_names()


saveRDS(file = "1. data/producao_u.rds", object = producao_u)

rm(list = ls())

# 1.2 Criação do df das informações poços Maritimos ==========================

pocos_M_2024 <- data.table::fread("0. data-raw/Tabela_pocos_2024_Maio_12.csv", encoding = "Latin-1") |> 
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
  )

saveRDS(file = "1. data/pocos_M_2024.rds", object = pocos_M_2024)

# 2.1 Graficos inicias com os dados dos poços ================================


fig_i_1 <- pocos_M_2024 |>
  group_by(Ano_inicio) |>
  summarise(count = n()) |>
  ggplot(aes(x = Ano_inicio, y = count))+
  geom_line(size = 2.5)+
  geom_point(size = 3.75) +
  ggthemes::theme_clean() +
  labs(
    x = NULL,
    y = NULL,
    title = "Exploração de novos poços de petróleo"
    #title = 'Ano: {frame_time}'
  ) +
  scale_x_continuous(breaks = seq(1954, 2024, by = 5)) +
  scale_y_continuous(breaks = seq(0, 300, by = 25)) +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 15, face = "italic"),
    axis.text.x = element_text(size = 18, face = "bold"),
    plot.background = element_rect(colour = "white")
  ) +
  transition_reveal(Ano_inicio)

fig_i_2 <- pocos_M_2024 |>
  group_by(Ano_inicio, recorte) |>
  summarise(count = n()) |>
  ggplot(aes(x = Ano_inicio, y = count, color = recorte))+
  geom_line(size = 2.5)+
  geom_point(size = 3.75) +
  ggthemes::theme_clean() +
  labs(
    x = NULL,
    y = NULL,
    title = "Exploração de novos poços de petróleo",
    color = NULL
  ) +
  scale_color_manual(
    values = c(
      MetBrewer::met.brewer("Homer2", 3)
    )
  ) +
  scale_x_continuous(breaks = seq(1954, 2024, by = 5)) +
  scale_y_continuous(breaks = seq(0, 300, by = 25)) +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 15, face = "italic"),
    axis.text.x = element_text(size = 18, face = "bold"),
    plot.background = element_rect(colour = "white")
  )

fig_i_3 <- pocos_M_2024 |>
  filter(Ano_inicio >= 2000) |>
  group_by(Ano_inicio, recorte, presal) |>
  summarise(count = n()) |>
  ggplot(aes(x = Ano_inicio, y = count, color = recorte)) +
  geom_line(size = 2.5)+
  geom_point(size = 3.75) +
  facet_wrap(.~presal, ncol = 2, scales = "free_x") +
  labs(
    x = NULL,
    y = NULL,
    title = "Exploração de novos poços de petróleo",
    color = NULL
  ) +
  ggthemes::theme_clean() +
  scale_color_manual(
    values = c(
      MetBrewer::met.brewer("Homer2", 3)
    )
  ) +
  scale_x_continuous(
    breaks = seq(2000, 2024, by = 4), 
    limits = c(1999, 2023)
  ) +
  scale_y_continuous(breaks = seq(0, 300, by = 25)) +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 12, face = "italic"),
    axis.text.x = element_text(size = 14, face = "bold"),
    plot.background = element_rect(colour = "white")
  )

ggsave("4. figs/fig_inicial_1.png", fig_i_1, dpi = 500, width = 12, height = 7)
ggsave("4. figs/fig_inicial_2.png", fig_i_2, dpi = 500, width = 12, height = 7)
ggsave("4. figs/fig_inicial_3.png", fig_i_3, dpi = 500, width = 12, height = 7)

# 2.2.1 Grafico dos das produção de barril por bacia =========================

producao_u <- readRDS("C:/Users/kaior/git/projeto_r_aula/1. data/producao_u.rds") 

#graf_barril_acumulado <-
producao_u |>
  group_by(periodo) |>
  summarise(sum_petroleo = sum(petroleo_bbl_dia)) |>
  mutate(ano_mes = ym(paste0(periodo))) |>
  ggplot(aes(x = ano_mes, y = sum_petroleo)) +
  geom_line(size = 1.5) + 
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    title = "Produção de barril de petroleo por dia, bacia de Campos/Santos-RJ (2005-2023)"
  ) +
  scale_y_continuous(
    breaks = seq(0, 5000000, by = 500000), 
    labels = scales::dollar_format(prefix = "", big.mark = ".", decimal.mark = ",")
  ) +
  scale_x_date(
    limits = as.Date(c("2005-01-01", "2023-12-31")),
    breaks = seq(as.Date("2005-01-01"), as.Date("2023-12-31"), by = "2 year"),
    date_labels = "%Y"
  ) +
  themes_plot()

#graf_barril_acumulado <-
  producao_u |>
  group_by(periodo, recorte) |>
  summarise(sum_petroleo = sum(petroleo_bbl_dia)) |>
  mutate(ano_mes = ym(paste0(periodo))) |>
  ggplot(aes(x = ano_mes, y = sum_petroleo, color = recorte)) +
  geom_line(size = 1.5) + 
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    title = "Produção de barril de petroleo por dia, bacia de Campos/Santos-RJ (2005-2023)"
  ) +
  scale_y_continuous(
    breaks = seq(0, 3000000, by = 500000), 
    labels = scales::dollar_format(prefix = "", big.mark = ".", decimal.mark = ",")
  ) +
  scale_x_date(
    limits = as.Date(c("2005-01-01", "2023-12-31")),
    breaks = seq(as.Date("2005-01-01"), as.Date("2023-12-31"), by = "2 year"),
    date_labels = "%Y"
  ) +
  pallet() +
  themes_plot() 

ggsave("4. figs/fig_barril.png", graf_barril, dpi = 500, width = 12, height = 7)


# 2.2.2 Left join e criação do gif dos dados dos poços com a produção por poço ============================

producao_u <- readRDS("C:/Users/kaior/git/projeto_r_aula/1. data/producao_u.rds") |>
  mutate(recorte=case_when(
    bacia =="Campos"~bacia,
    bacia =="Santos"~bacia,
    TRUE ~ "Outros"
  ))
  

state_br <- geobr::read_state(year = 2010)

pocos_M_2024 <- data.table::fread("0. data-raw/Tabela_pocos_2024_Maio_12.csv", encoding = "Latin-1") |> 
  filter(TERRA_MAR=="M") |>
  mutate(
    data_conclusao = lubridate::dmy(CONCLUSAO),
    Ano_inicio = lubridate::year(dmy(INICIO)),
    Ano_termino = lubridate::year(dmy(TERMINO)),
    Ano_conclusao = lubridate::year(dmy(CONCLUSAO)),
  ) |>
  mutate(LATITUDE_BASE_DD = as.character(LATITUDE_BASE_DD),
         LONGITUDE_BASE_DD = as.character(LONGITUDE_BASE_DD)) %>%
  mutate(LATITUDE_BASE_DD = stri_replace_all_fixed(LATITUDE_BASE_DD, ",", "."),
         LONGITUDE_BASE_DD = stri_replace_all_fixed(LONGITUDE_BASE_DD, ",", ".")) |>
  mutate(presal= case_when(
    ATINGIU_PRESAL== "S" ~ 1,
    TRUE ~ 0)
  ) |> 
  mutate(recorte=case_when(
    BACIA =="Campos"~BACIA,
    BACIA =="Santos"~BACIA,
    TRUE ~ "Outros"
  )) |>
  janitor::clean_names()

dados <- producao_u |>
  filter(petroleo_bbl_dia > 1) |>
  left_join(pocos_M_2024, by= c("nome_poco_anp"="poco","recorte")) 

dados <- dados |>
  mutate(ano_mes = ym(paste0(periodo)))

dados$profundidade_numeric <- as.numeric(stri_replace_all_fixed(dados$profundidade_vertical_m, ",", "."))

dados |>
  filter(petroleo_bbl_dia > 1) |>
  filter(profundidade_numeric < 0) |>
  ggplot(aes(x = profundidade_numeric, y = petroleo_bbl_dia)) +
  geom_point(aes(color = as.character(presal))) + 
  geom_smooth()

gif_map1 <- 
  ggplot() +
  geom_sf(data = state_br) + 
  geom_point(data = dados,
    aes(
      x = as.numeric(longitude_base_dd),
      y = as.numeric(latitude_base_dd),
      size = petroleo_bbl_dia,
      color = petroleo_bbl_dia
    )
  ) +
  coord_sf(
    xlim = c(-47.0, -38.0),
    ylim = c(-27.0, -20.00),
    expand = FALSE
  ) +
  labs(
    title = "Ano: {frame_time}",
    subtitle = "Frame {frame} of {nframes}",
    x = NULL, 
    y = NULL
  ) +
  theme_classic() +
  scale_color_distiller(palette = "Spectral", direction = 1) + 
  gganimate::transition_time(dados$ano_mes)

gi_map1_f <- gganimate::animate(gif_map1, nframes = 25, fps = 5)

gganimate::anim_save("4. figs/map_anos.gif", gi_map1_f)

# 2.3 Indice de produtividade por poço =======================================

#pocos_M_2024[conclusao == ""]  <- "2024-05-01"

producao_u <- readRDS("C:/Users/kaior/git/projeto_r_aula/1. data/producao_u.rds")

pocos_M_2024$dias_funcionando <- as.numeric(difftime(lubridate::dmy(pocos_M_2024$conclusao), lubridate::dmy(pocos_M_2024$inicio), units = "days"))

prod_data <- producao_u |>
  mutate(recorte=case_when(
    bacia == "Campos" ~ bacia, 
    bacia == "Santos" ~ bacia, 
    TRUE ~ "Outros"
  )) |>
  #filter(petroleo_bbl_dia > 1) |>
  group_by(nome_poco_anp, recorte) |>
  summarise(sum_petroleo = sum(petroleo_bbl_dia)) |>
  ungroup()|>
  left_join(pocos_M_2024, by= c("nome_poco_anp"="poco","recorte")) 

prod_data |> 
  mutate(produtividade_poco = sum_petroleo / dias_funcionando) |>
  filter(dias_funcionando > 0 & sum_petroleo > 0) |>
  group_by(recorte, campo) |>
  summarise(mean_produtividade = mean(produtividade_poco)) |>
  arrange(-mean_produtividade) |>
  head(35) |>
  mutate(nome_poco = paste0(campo, "/", recorte)) |>
  ggplot(aes(x = mean_produtividade, y = reorder(nome_poco, mean_produtividade), color = recorte)) +
    geom_point(size = 4) +
    pallet() +
    themes_plot() 
  

#fig_prod <- 

prod_data |> 
  mutate(produtividade_poco = sum_petroleo / dias_funcionando) |>
  filter(dias_funcionando > 0 & sum_petroleo > 0) |>
  group_by(recorte, campo) |>
  summarise(
    count = n(),
    mean_produtividade = mean(produtividade_poco),
    sem_produtividade = sd(produtividade_poco) / sqrt(n())
  ) |>
  arrange(-mean_produtividade) |>
  head(15) |>
  mutate(nome_poco = paste0(campo, "/", recorte)) |>
  ggplot(aes(x = mean_produtividade, y = reorder(nome_poco, mean_produtividade), color = recorte)) +
  geom_point(size = 4) + #, position = position_dodge2(width = 15)
  geom_errorbar(aes(xmin = mean_produtividade - sem_produtividade, xmax = mean_produtividade + sem_produtividade), width = 0.2) +
  geom_text(size = 4.0, color = "black", nudge_y = 0.6, aes(label = glue("N: {count}"))) +
  geom_text(size = 3.25, color = "black", nudge_y = 0.3, aes(label = paste0("(", round(mean_produtividade, digits = 2), ")"))) +
  labs(x = NULL, y = NULL) +
  ggthemes::theme_clean() + 
  scale_color_manual(
    values = c(
      MetBrewer::met.brewer("Homer2", 3)
    )
  ) +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 10, face = "italic"),
    axis.text.x = element_text(size = 12, face = "bold"),
    plot.background = element_rect(colour = "white")
  )

fig_prod2 <- prod_data |> 
  mutate(produtividade_poco = sum_petroleo / dias_funcionando) |>
  filter(dias_funcionando > 0 & sum_petroleo > 0) |>
  group_by(recorte, campo, operador) |>
  summarise(
    count = n(),
    mean_produtividade = mean(produtividade_poco),
    sem_produtividade = sd(produtividade_poco) / sqrt(n())
  ) |>
  arrange(-mean_produtividade) |>
  head(15) |>
  mutate(nome_poco = paste0(campo, "/", recorte)) |>
  ggplot(aes(x = mean_produtividade, y = reorder(nome_poco, mean_produtividade), color = operador)) +
  geom_point(size = 4) + 
  labs(x = NULL, y = NULL) +
  ggthemes::theme_clean() + 
  scale_color_manual(
    values = c(
      MetBrewer::met.brewer("Homer2", 5, direction = -1, override.order =  TRUE)
    )
  ) +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 10, face = "italic"),
    axis.text.x = element_text(size = 12, face = "bold"),
    plot.background = element_rect(colour = "white")
  )

ggsave(plot = fig_prod, filename = "4. figs/fig_produtividade.png", 
       dpi = 500, 
       width = 12, 
       height = 8)

ggsave(plot = fig_prod2, filename = "4. figs/fig_produtividade2.png", 
       dpi = 500, 
       width = 12, 
       height = 8)

prod_data |> 
  filter(dias_funcionando > 0 & sum_petroleo > 0) |>
  group_by(recorte, operador) %>%
  summarise(
    count = n(),
    sum_petroleo = sum(sum_petroleo),
  ) |> 
  mutate(
    petro_percent = (sum_petroleo / sum(sum_petroleo)) * 100,
    var_petrobras = case_when(
      operador == "Petrobras" ~ "Petrobras",
      TRUE ~ "Outros")
  )|>
  ggplot(aes(x = sum_petroleo, y = reorder(recorte, sum_petroleo), fill = var_petrobras)) +
  geom_col() +
  geom_text(aes(label = round(petro_percent, digits = 2)))

