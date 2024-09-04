# Pacotes
library(tidyverse)
library(plm)
library(stargazer)
library(lubridate)
library(zoo)

# Leitura da base de dados
base <- readRDS("analysis/dados/base.RDS")

# Criando o data frame com os dados do PIB
pib <- data.frame(
  ano = as.character(2003:2021),
  pib_br = c(1.1, 5.8, 3.2, 4.0, 6.1, 5.1, -0.1, 7.5, 4.0, 1.9, 3.0, 0.5, -3.5, -3.3, 1.3, 1.8, 1.2, -3.3, 4.8),
  pib_df = c(0.7, 5.0, 5.8, 5.5, 6.6, 4.5, 5.0, 4.4, 3.7, 0.8, 3.7, 2.0, -1.0, 0.0, 0.3, 1.7, 2.1,-2.6,3.0)
)

# Definindo as variáveis de intervalo
buraco_inicio <- as.Date("2013-09-01")
buraco_fim <- as.Date("2014-09-01")

# Função para preparação e plotagem dos dados
plot_mm3 <- function(base, y_var, title) {
  base |>
    mutate(grupo_20 = as.factor(grupo_20),
           ano_mes = paste0(base$ano,"-",ifelse(base$mes %in% 10:19,base$mes,paste0(0,base$mes))) |> ym()) |>
    group_by(grupo_20, ano_mes) |>
    summarise(media = mean({{ y_var }}, na.rm = TRUE)) |>
    arrange(grupo_20, ano_mes) |>
    mutate(mm_3 = zoo::rollmean(media, 12, fill = NA),
           # Introduzindo NAs no intervalo desejado
           mm_3 = ifelse(ano_mes >= buraco_inicio & ano_mes <= buraco_fim, NA, mm_3)) |>
    right_join(data.frame(ano_mes = seq(from = as.Date("2009-01-01"), 
                                        to = as.Date("2019-08-01"), 
                                        by = "month"))) |>
    ggplot(aes(x = ano_mes, y = mm_3, col = grupo_20)) +
    scale_x_date(breaks = "2 years") +
    geom_line(lwd = 1, na.rm = TRUE) + 
    scale_color_manual(values = c("grey10","grey")) +
    annotate("rect", xmin = as.Date("2013-08-01"), xmax = as.Date("2014-10-01"), ymin = -Inf, ymax = Inf, 
             alpha = 0.2, fill = "grey50") + 
    geom_vline(xintercept = as.Date("2014-06-14"), linetype = "dashed", color = "black", alpha = .9) +
    theme_linedraw() + ggtitle(title)
}

# Plotando os gráficos
plot_mm3(base, ocupado, "Ocupado")
plot_mm3(base |> filter(!is.na(ocupado)), informal, "Informal")
plot_mm3(base |> filter(!is.na(ocupado)), rend_bruto, "Rendimento")
plot_mm3(base |> filter(!is.na(ocupado)), idade, "Idade")
plot_mm3(base |> filter(!is.na(ocupado)) , trab_plano, "Trabalha no Plano")
