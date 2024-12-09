# Pacotes
library(tidyverse)
library(plm)
library(stargazer)
library(lubridate)
library(sidrar)
library(zoo)

# Leitura da base_gama de dados
base<- readRDS("analysis/dados/base_ra.RDS") |> filter(reg %in% c("Gama","Santa Maria","Recanto Das Emas","Sobradinho"))

base_gama <- base_gama |> filter(reg %in% c("Gama","Recanto Das Emas")) |> 
  mutate(grupo = ifelse(reg == "Gama",1,0))

base_sm <- base |> filter(reg %in% c("Santa Maria","Sobradinho")) |> 
  mutate(grupo = ifelse(reg == "Santa Maria",1,0))

# base_gama Gama ----

tab1_gama <- base_gama |>
  mutate(grupo = ifelse(grupo == 1,"Treated","Control"),
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

tabela_gama <- cbind(tab1_gama |> filter(periodo == "Pre-BRT") |> select(-periodo),
                tab1_gama |> filter(periodo == "Post-BRT")|> select(-periodo)) |> 
  kableExtra::kable(col.names = c("Variável", "Control Mean", "Control SD", "Treated Mean", "Treated SD", 
                                  "Variável", "Control Mean", "Control SD", "Treated Mean", "Treated SD"),
                    align = "lccccclcccc") |>
  kableExtra::add_header_above(c(" " = 2, "Pre-BRT" = 4, "Post-BRT" = 4)) |>
  kableExtra::row_spec(0, bold = TRUE, font_size = 12) |>
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

tabela_gama

# base_gama Santa Maria ----

tab1_sm <- base_sm |>
  mutate(grupo = ifelse(grupo == 1,"Treated","Control"),
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

tabela_sm <- cbind(tab1_sm |> filter(periodo == "Pre-BRT") |> select(-periodo),
                     tab1_sm |> filter(periodo == "Post-BRT")|> select(-periodo)) |> 
  kableExtra::kable(col.names = c("Variável", "Control Mean", "Control SD", "Treated Mean", "Treated SD", 
                                  "Variável", "Control Mean", "Control SD", "Treated Mean", "Treated SD"),
                    align = "lccccclcccc") |>
  kableExtra::add_header_above(c(" " = 2, "Pre-BRT" = 4, "Post-BRT" = 4)) |>
  kableExtra::row_spec(0, bold = TRUE, font_size = 12) |>
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

tabela_sm



# Função para criar os gráficos
criar_grafico <- function(base, variavel, titulo, legend = FALSE) {
  base |> 
    filter(!is.na(ocupado)) |> 
    mutate(grupo = ifelse(grupo == 1, "Treated", "Control")) |> 
    group_by(ano, grupo) |>
    summarise(media = mean({{ variavel }}, na.rm = TRUE), .groups = "drop") |> 
    ggplot(aes(x = as.character(ano), y = media, col = grupo, group = grupo)) +
    geom_line(lwd = 1, show.legend = legend) + 
    scale_color_manual(values = c("grey10", "grey")) +
    geom_vline(xintercept = "2014", linetype = "dashed", color = "black", alpha = .9) +
    theme_linedraw() + 
    ggtitle(titulo)
}

# Criando gráficos de base_gama
g0 <- criar_grafico(base_gama, ocupado, "Grupos", legend = TRUE)
g1 <- criar_grafico(base_gama, ocupado, "Percentual de Ocupados")
g2 <- criar_grafico(base_gama, informal, "Percentual de Informais")
g3 <- criar_grafico(base_gama, en_sup, "Percentual de Superior Completo")
g5 <- criar_grafico(base_gama, rend_bruto, "Rendimento Bruto Médio")
g7 <- criar_grafico(base_gama, horas_trab, "Horas Trabalhadas")
g8 <- criar_grafico(base_gama, trab_plano, "Trabalha no Plano Piloto")

# Combinação de gráficos
grafico_gama <- g0 / (g1 + g2) / (g3 + g5) / (g7 + g8)

# Criando gráficos de base_sm
sm_g0 <- criar_grafico(base_sm, ocupado, "Grupos", legend = TRUE)
sm_g1 <- criar_grafico(base_sm, ocupado, "Percentual de Ocupados")
sm_g2 <- criar_grafico(base_sm, informal, "Percentual de Informais")
sm_g3 <- criar_grafico(base_sm, en_sup, "Percentual de Superior Completo")
sm_g5 <- criar_grafico(base_sm, rend_bruto, "Rendimento Bruto Médio")
sm_g7 <- criar_grafico(base_sm, horas_trab, "Horas Trabalhadas")
sm_g8 <- criar_grafico(base_sm, trab_plano, "Trabalha no Plano Piloto")

# Combinação de gráficos
grafico_sm <- sm_g0 / (sm_g1 + sm_g2) / (sm_g3 + sm_g5) / (sm_g7 + sm_g8)

# Exibição dos gráficos
grafico_gama
grafico_sm
