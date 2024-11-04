# Pacotes
library(tidyverse)
library(plm)
library(stargazer)
library(lubridate)
library(sidrar)
library(zoo)

# Leitura da base de dados
base <- readRDS("analysis/dados/base.RDS") #|> filter(NM_SUBDIST == "SANTA MARIA")

tab1 <- base |>
  mutate(grupo = ifelse(grupo_20 == 1,"Treated","Control"),
         periodo = ifelse(aamm < 201406, "Pre-BRT", "Post-BRT")) |>
  group_by(grupo, periodo) |>
  summarise(
    Ocupados_Mean = mean(ocupado, na.rm = TRUE),
    Ocupados_SD = sd(ocupado, na.rm = TRUE),
    Informais_Mean = mean(informal, na.rm = TRUE),
    Informais_SD = sd(informal, na.rm = TRUE),
    Rendimento_Mean = mean(rend_bruto, na.rm = TRUE),
    Rendimento_SD = sd(rend_bruto, na.rm = TRUE),
    Horas_Mean = mean(horas_trab, na.rm = TRUE),
    Horas_SD = sd(horas_trab, na.rm = TRUE),
    Moradores_Mean = mean(pessoas, na.rm = TRUE),
    Moradores_SD = sd(pessoas, na.rm = TRUE)
  ) |>
  pivot_longer(cols = Ocupados_Mean:Moradores_SD, names_to = "Variável", values_to = "Valor") |>
  separate(Variável, into = c("Variável", "Métrica")) |> 
  pivot_wider(names_from = c(grupo, Métrica), values_from = Valor)

tabela <- cbind(tab1 |> filter(periodo == "Pre-BRT") |> select(-periodo),
                tab1 |> filter(periodo == "Post-BRT")|> select(-periodo)) |> 
  kableExtra::kable(col.names = c("Variável", "Control Mean", "Control SD", "Treated Mean", "Treated SD", 
                                  "Variável", "Control Mean", "Control SD", "Treated Mean", "Treated SD"),
                    align = "lccccclcccc") |>
  kableExtra::add_header_above(c(" " = 2, "Pre-BRT" = 4, "Post-BRT" = 4)) |>
  kableExtra::row_spec(0, bold = TRUE, font_size = 12) |>
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

tabela

# Gráficos ----

base_g0 <- base |> 
  filter(!is.na(ocupado)) |> 
  mutate(grupo = ifelse(grupo_20 == 1,"Treated","Control")) |> 
  group_by(ano,grupo) |>
  summarise(n = n()) 

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
  summarise(media = mean(en_sup, na.rm = TRUE)) 

g3 <- base_g3 |> 
  ggplot(aes(x = ano, y = media,col = grupo, group = grupo)) +
  geom_line(lwd = 1) + 
  scale_color_manual(values = c("grey10","grey")) +
  # annotate("rect", xmin = "2013.8", xmax = "2014.8", ymin = -Inf, ymax = Inf, 
  #          alpha = 0.2, fill = "grey50") + 
  geom_vline(xintercept = "2014", linetype = "dashed", color = "black", alpha = .9) +
  theme_linedraw() + ggtitle("Percentual de Superior Completo")

g3


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


