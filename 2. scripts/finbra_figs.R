
pacman::p_load(
  tidyverse,
  stringi
)

f_unificiado <- readRDS("1. data/f_unificiado.rds")

(g_1 <- f_unificiado |>
  group_by(munic) |>
  summarise(gasto_saude = sum(x10_saude, na.rm = TRUE),
    tota_despesa = sum(total_geral_da_despesa, na.rm = TRUE)) |>
  mutate(percentual_saude = gasto_saude/tota_despesa, munic = paste0(munic, "/RJ")) |>
  arrange(-percentual_saude) |>
  head(15) |>
  ggplot(aes(x=percentual_saude, y=reorder(munic, percentual_saude),fill=percentual_saude)) +
    geom_bar(stat = "identity") +
    labs(y=NULL) +
    scale_fill_distiller(palette = "Spectral", direction = 1) +
    coord_cartesian(expand = F, ylim = c(0.25,15.75))+
    ggthemes::theme_clean() +
    theme(legend.position = "none")
)


municipios_graph <- c(
  "Campos dos Goytacazes", "Macaé", 
  "Niterói", "Rio de Janeiro", 
  "São João da Barra", "São Francisco de Itabapoana", 
  "Petrópolis", "Maricá"
)

media_geral <- 
f_unificiado |>
  mutate(percentual_saude = x10_saude/total_geral_da_despesa)|>
  group_by(nu_ano) |>
  summarise(gasto_saude = median(x10_saude, na.rm = TRUE),
    tota_despesa = median(total_geral_da_despesa, na.rm = TRUE)) |>
  mutate(percentual_saude = gasto_saude/tota_despesa, conta="Mediana Estadual") |> 
  cross_join(municipios_graph, copy = T) |>
  rename(munic=y)

(g_2 <- 
f_unificiado |>
  group_by(nu_ano, munic) |>
  summarise(gasto_saude = sum(x10_saude, na.rm = TRUE),
    tota_despesa = sum(total_geral_da_despesa, na.rm = TRUE)) |>
  filter(munic %in% c(municipios_graph)
  ) |>
  mutate(percentual_saude = gasto_saude/tota_despesa, 
         percentual_saude = case_when(
           percentual_saude == 0 ~ NA,
           TRUE ~ percentual_saude
         )) |>
  bind_rows(media_geral) |> 
  mutate(munic = paste0(munic, "/RJ"),
         conta = case_when(
           is.na(conta) ~ "Saúde",
           TRUE ~ conta)
         ) |>
  ggplot(aes(x=nu_ano, y=percentual_saude, color=conta, linetype=conta)) +
    geom_point(size=2.25) +
    geom_line(size=0.975) +
    labs(x=NULL, y=NULL, color=NULL, linetype=NULL,
         title = "Percentuais do Gasto Municipal em Saúde")+
    facet_wrap(.~munic, scales = "free") +
    ggthemes::theme_clean() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(face = "italic"),
          axis.text.y = element_text(face = "italic")) +
    scale_color_manual(
      values = c(
        "black",
        "red"
      )
    )+
    scale_linetype_manual(
      values = c(
        "dashed",
        "solid"
      )
    )
) 
