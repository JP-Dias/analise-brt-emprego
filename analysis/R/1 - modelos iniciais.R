# Pacotes
library(tidyverse)
library(plm)
library(stargazer)

base <- readRDS("analysis/dados/base.RDS")

# Criando o data frame com os dados do PIB
pib <- data.frame(
  ano = as.character(2003:2021),
  pib_br = c(1.1, 5.8, 3.2, 4.0, 6.1, 5.1, -0.1, 7.5, 4.0, 1.9, 3.0, 0.5, -3.5, -3.3, 1.3, 1.8, 1.2, -3.3, 4.8),
  pib_df = c(0.7, 5.0, 5.8, 5.5, 6.6, 4.5, 5.0, 4.4, 3.7, 0.8, 3.7, 2.0, -1.0, 0.0, 0.3, 1.7, 2.1,-2.6,3.0)
)

data <- base |> 
  filter(!is.na(ocupado),
         !is.na(escol_sup_com)) |> 
  mutate(
    intervencao_14_15 = ifelse(ano %in% c("2014","2015"),1,0),
    intervencao_15_17 = ifelse(ano %in% c("2015","2016","2017"),1,0),
    intervencao_17_19 = ifelse(ano %in% c("2017","2018","2019"),1,0),
         ) |> 
  select(ocupado,informal,ln_rend_bruto,ln_horas_trab,
         intervencao, 
         intervencao_14_15,
         intervencao_15_17,
         intervencao_17_19,
         trat20 = grupo_20,trat30 = grupo_30,
         ano,mes,ra = NM_SUBDIST,
         idade, escol_sup_com) |> 
  left_join(pib)


m1 <- lm(formula = ocupado ~ trat20 + intervencao_14_15 + intervencao_15_17 + intervencao_17_19 + 
                   (trat20 * intervencao_14_15) + (trat20 * intervencao_15_17) + (trat20 * intervencao_17_19) -1,
                 data = data,
         weights = data$fator) 

m2 <- lm(formula = informal ~ trat20 + intervencao_14_15 + intervencao_15_17 + intervencao_17_19 + 
           (trat20 * intervencao_14_15) + (trat20 * intervencao_15_17) + (trat20 * intervencao_17_19) + pib_df,
         data = data) 
 
stargazer(m1, type = "text")
stargazer(m2, type = "text")
 
 modelo_2 <- plm(formula = ocupado ~ trat20 + intervencao + (trat20 * intervencao),
                 model = "within",
                 index = c("ano"),
                 data = data) 
 
 modelo_3 <- lm(formula = ocupado ~ trat20 + intervencao + (trat20 * intervencao) + pib_df,
                data = data) 
 
stargazer(modelo_3, type = "text")


base |> group_by(ano) |> summarise(prop=mean(ocupado,na.rm = T))
 
base |> 
  group_by(intervencao,grupo_20) |> 
  summarise(emprego = mean(ocupado,na.rm = T),
            horas_trab = mean(horas_trab,na.rm = T),
            rend = mean(rend_bruto,na.rm = T),
            mulher = mean(fem),
            sup = mean(escol_sup_com,na.rm = T),
            chefe = mean(posicao_fam_chefe)
            )

base |>
  mutate(grupo_20 = as.factor(grupo_20),
         ano = as.numeric(as.character(ano))) |>  # Convert ano to numeric
  group_by(ano, grupo_20) |>
  summarise(media = mean(rend_bruto, na.rm = TRUE)) |>
  ggplot(aes(x = ano, y = media, col = grupo_20, group = grupo_20)) +
  geom_line(lwd = 1) + 
  geom_vline(xintercept = 2014, linetype = "dashed", color = "black",alpha = .5) +
  theme_bw()

base |>
  mutate(grupo_20 = as.factor(grupo_20),
         ano = as.numeric(as.character(ano))) |>  # Convert ano to numeric
  group_by(ano, grupo_20) |>
  summarise(media = mean(horas_trab, na.rm = TRUE)) |>
  ggplot(aes(x = ano, y = media, col = grupo_20, group = grupo_20)) +
  geom_line(lwd = 1) + 
  geom_vline(xintercept = 2014, linetype = "dashed", color = "black",alpha = .5) +
  theme_bw()

base |>
  mutate(grupo_20 = as.factor(grupo_20),
         ano = as.numeric(as.character(ano))) |>  # Convert ano to numeric
  group_by(ano, grupo_20) |>
  summarise(media = mean(ocupado,na.rm = T)) |>
  ggplot(aes(x = ano, y = media, col = grupo_20, group = grupo_20)) +
  geom_line(lwd = 1) + 
  geom_vline(xintercept = 2014, linetype = "dashed", color = "black",alpha = .5) +
  theme_bw()


base |>
  mutate(grupo_20 = as.factor(grupo_20),
         ano = as.numeric(as.character(ano))) |>  # Convert ano to numeric
  group_by(ano, grupo_20) |>
  summarise(media = mean(informal,na.rm = T)) |>
  ggplot(aes(x = ano, y = media, col = grupo_20, group = grupo_20)) +
  geom_line(lwd = 1) + 
  geom_vline(xintercept = 2014, linetype = "dashed", color = "black",alpha = .5) +
  theme_bw()

base |>
  mutate(grupo_20 = as.factor(grupo_20),
         ano = as.numeric(as.character(ano))) |>  # Convert ano to numeric
  group_by(ano, grupo_20) |>
  summarise(media = mean(escol_sup_com,na.rm = T)) |>
  ggplot(aes(x = factor(ano), y = media, col = grupo_20, group = grupo_20, linetype = grupo_20)) +
  geom_line(lwd = 1) + 
  scale_color_manual(values = c("grey10","grey40")) +
  geom_vline(xintercept = "2014", linetype = "dashed", color = "black",alpha = .5) +
  theme_linedraw()


base |>
  mutate(grupo_20 = as.factor(grupo_20),
         ano_mes = paste0(base$ano,"-",ifelse(base$mes %in% 10:19,base$mes,paste0(0,base$mes))) |> lubridate::ym()) |>  # Convert ano to numeric
  group_by(grupo_20,ano_mes) |>
  summarise(media = mean(ocupado,na.rm = T)) |>
  arrange(grupo_20,ano_mes) |>
  mutate(mm_3 = zoo::rollmean(media,12,fill = NA)) |>
  ggplot(aes(x = ano_mes, y = mm_3, col = grupo_20, group = grupo_20, linetype = grupo_20)) +
  scale_x_date(breaks = "2 years") +
  geom_line(lwd = 1) + 
  scale_color_manual(values = c("grey10","grey40")) +
  geom_vline(xintercept = lubridate::ym(2014-06), linetype = "dashed", color = "black",alpha = .5) +
  theme_linedraw()

base |> 
  mutate(grupo_20 = as.factor(grupo_20)) |> 
  group_by(ano,grupo_20) |> 
  summarise(media = mean(rend_liquido,na.rm = T)) |> view()

