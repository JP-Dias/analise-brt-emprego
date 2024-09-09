# Pacotes
library(tidyverse)
library(plm)
library(stargazer)
library(lubridate)
library(zoo)

# Leitura da base de dados
base <- readRDS("analysis/dados/base.RDS") #|> filter(NM_SUBDIST == "SANTA MARIA")

# Grupos ----

base_g0 <- base |> 
  filter(!is.na(ocupado)) |> 
  mutate(grupo = ifelse(grupo_20 == 1,"Treated","Control")) |> 
  group_by(ano,grupo) |>
  summarise(n = n()) 

print(base_g0,n = Inf)

g0 <- base_g0 |> 
  ggplot(aes(x = ano, y = n,col = grupo, group = grupo)) +
  geom_line(lwd = 1) + 
  scale_color_manual(values = c("grey10","grey")) +
  # annotate("rect", xmin = "2013.8", xmax = "2014.8", ymin = -Inf, ymax = Inf, 
  #          alpha = 0.2, fill = "grey50") + 
  geom_vline(xintercept = "2014", linetype = "dashed", color = "black", alpha = .9) +
  theme_linedraw() + ggtitle("Grupos")

g0


# Ocupados ----
base_g1 <- base |> 
  filter(!is.na(ocupado)) |> 
  mutate(grupo = ifelse(grupo_20 == 1,"Treated","Control")) |> 
  group_by(ano,grupo) |>
  summarise(media = mean(ocupado, na.rm = TRUE)) 

g1 <- base_g1 |> 
  ggplot(aes(x = ano, y = media,col = grupo, group = grupo)) +
  geom_line(lwd = 1) + 
  scale_color_manual(values = c("grey10","grey")) +
  # annotate("rect", xmin = "2013.8", xmax = "2014.8", ymin = -Inf, ymax = Inf, 
  #          alpha = 0.2, fill = "grey50") + 
  geom_vline(xintercept = "2014", linetype = "dashed", color = "black", alpha = .9) +
  theme_linedraw() + ggtitle("Percentual de Ocupados")

g1

# Informais ----

base_g2 <- base |> 
  filter(!is.na(ocupado)) |> 
  mutate(grupo = ifelse(grupo_20 == 1,"Treated","Control")) |> 
  group_by(ano,grupo) |>
  summarise(media = mean(informal, na.rm = TRUE)) 

g2 <- base_g2 |> 
  ggplot(aes(x = ano, y = media,col = grupo, group = grupo)) +
  geom_line(lwd = 1) + 
  scale_color_manual(values = c("grey10","grey")) +
  # annotate("rect", xmin = "2013.8", xmax = "2014.8", ymin = -Inf, ymax = Inf, 
  #          alpha = 0.2, fill = "grey50") + 
  geom_vline(xintercept = "2014", linetype = "dashed", color = "black", alpha = .9) +
  theme_linedraw() + ggtitle("Percentual de Informais")

g2


# Escolaridade ----
# Ensino Superior
base_g3 <- base |> 
  filter(!is.na(ocupado)) |> 
  mutate(grupo = ifelse(grupo_20 == 1,"Treated","Control")) |> 
  group_by(ano,grupo) |>
  summarise(media = mean(escol_sup_com, na.rm = TRUE)) 

g3 <- base_g3 |> 
  ggplot(aes(x = ano, y = media,col = grupo, group = grupo)) +
  geom_line(lwd = 1) + 
  scale_color_manual(values = c("grey10","grey")) +
  # annotate("rect", xmin = "2013.8", xmax = "2014.8", ymin = -Inf, ymax = Inf, 
  #          alpha = 0.2, fill = "grey50") + 
  geom_vline(xintercept = "2014", linetype = "dashed", color = "black", alpha = .9) +
  theme_linedraw() + ggtitle("Percentual de Superior Completo")

g3

# Ensino Médio

base_g4 <- base |> 
  filter(!is.na(ocupado)) |> 
  mutate(grupo = ifelse(grupo_20 == 1,"Treated","Control")) |> 
  group_by(ano,grupo) |>
  summarise(media = mean(escol_med_com, na.rm = TRUE)) 

g4 <- base_g3 |> 
  ggplot(aes(x = ano, y = media,col = grupo, group = grupo)) +
  geom_line(lwd = 1) + 
  scale_color_manual(values = c("grey10","grey")) +
  # annotate("rect", xmin = "2013.8", xmax = "2014.8", ymin = -Inf, ymax = Inf, 
  #          alpha = 0.2, fill = "grey50") + 
  geom_vline(xintercept = "2014", linetype = "dashed", color = "black", alpha = .9) +
  theme_linedraw() + ggtitle("Percentual de Médio Completo")

g4

# Rendimento ----

base_g5 <- base |> 
  filter(!is.na(ocupado)) |> 
  mutate(grupo = ifelse(grupo_20 == 1,"Treated","Control")) |> 
  group_by(ano,grupo) |>
  summarise(media = mean(rend_bruto, na.rm = TRUE)) 

g5 <- base_g5 |> 
  ggplot(aes(x = ano, y = media,col = grupo, group = grupo)) +
  geom_line(lwd = 1) + 
  scale_color_manual(values = c("grey10","grey")) +
  # annotate("rect", xmin = "2013.8", xmax = "2014.8", ymin = -Inf, ymax = Inf, 
  #          alpha = 0.2, fill = "grey50") + 
  geom_vline(xintercept = "2014", linetype = "dashed", color = "black", alpha = .9) +
  theme_linedraw() + ggtitle("Rendimento Bruto Médio")

g5


base_g6 <- base |> 
  filter(!is.na(ocupado)) |> 
  mutate(grupo = ifelse(grupo_20 == 1,"Treated","Control")) |> 
  group_by(ano,grupo) |>
  summarise(media = mean(rend_liquido, na.rm = TRUE)) 

g6 <- base_g5 |> 
  ggplot(aes(x = ano, y = media,col = grupo, group = grupo)) +
  geom_line(lwd = 1) + 
  scale_color_manual(values = c("grey10","grey")) +
  # annotate("rect", xmin = "2013.8", xmax = "2014.8", ymin = -Inf, ymax = Inf, 
  #          alpha = 0.2, fill = "grey50") + 
  geom_vline(xintercept = "2014", linetype = "dashed", color = "black", alpha = .9) +
  theme_linedraw() + ggtitle("Rendimento Líquido Médio")

g6

# Horas ----

base_g7 <- base |> 
  filter(!is.na(ocupado)) |> 
  mutate(grupo = ifelse(grupo_20 == 1,"Treated","Control")) |> 
  group_by(ano,grupo) |>
  summarise(media = mean(horas_trab, na.rm = TRUE)) 

g7 <- base_g7 |> 
  ggplot(aes(x = ano, y = media,col = grupo, group = grupo)) +
  geom_line(lwd = 1) + 
  scale_color_manual(values = c("grey10","grey")) +
  # annotate("rect", xmin = "2013.8", xmax = "2014.8", ymin = -Inf, ymax = Inf, 
  #          alpha = 0.2, fill = "grey50") + 
  geom_vline(xintercept = "2014", linetype = "dashed", color = "black", alpha = .9) +
  theme_linedraw() + ggtitle("Horas Trabalhadas")

g7


# Trabaha no Plano ----

base_g8 <- base |> 
  filter(!is.na(ocupado)) |> 
  mutate(grupo = ifelse(grupo_20 == 1,"Treated","Control")) |> 
  group_by(ano,grupo) |>
  summarise(media = mean(trab_plano, na.rm = TRUE)) 

g8 <- base_g8 |> 
  ggplot(aes(x = ano, y = media,col = grupo, group = grupo)) +
  geom_line(lwd = 1) + 
  scale_color_manual(values = c("grey10","grey")) +
  # annotate("rect", xmin = "2013.8", xmax = "2014.8", ymin = -Inf, ymax = Inf, 
  #          alpha = 0.2, fill = "grey50") + 
  geom_vline(xintercept = "2014", linetype = "dashed", color = "black", alpha = .9) +
  theme_linedraw() + ggtitle("Trabalha no Plano Piloto")

g8


