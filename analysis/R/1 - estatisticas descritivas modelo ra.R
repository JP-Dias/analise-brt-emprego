# Pacotes
library(tidyverse)
library(plm)
library(stargazer)
library(lubridate)
library(sidrar)
library(zoo)
library(Hmisc)
library(fastDummies)

# Leitura da base_gama de dados
base<- readRDS("analysis/dados/base_ra.RDS") 

base_gama <- base |> filter(reg %in% c("Gama","Ceilândia"),ano>2009) |> 
  mutate(grupo = ifelse(reg == "Gama",1,0)) |> 
  dummy_cols("setor_atv")

base_sm <- base |> filter(reg %in% c("Santa Maria","Recanto Das Emas"),ano>2009) |> 
  mutate(grupo = ifelse(reg == "Santa Maria",1,0)) |> 
  dummy_cols("setor_atv")

# base_gama Gama ----

tab1_gama <- base_gama |>
  mutate(grupo = ifelse(grupo == 1,"Treated","Control"),
         periodo = ifelse(aamm < 201406, "Pre-BRT", "Post-BRT")) |>
  filter(periodo == "Pre-BRT") |> 
  group_by(grupo, periodo) |>
  summarise(
    Ocupados_Mean = wtd.mean(ocupado,peso, na.rm = TRUE),  
    Ocupados_SD = sqrt(wtd.var(ocupado,peso, na.rm = TRUE)),
    Informais_Mean = wtd.mean(informal,peso, na.rm = TRUE),
    Informais_SD = sqrt(wtd.var(informal,peso, na.rm = TRUE)),
    Rendimento_Mean = wtd.mean(rend_bruto,peso, na.rm = TRUE),
    Rendimento_SD = sqrt(wtd.var(rend_bruto,peso, na.rm = TRUE)),
    Horas_Mean = wtd.mean(horas_trab,peso, na.rm = TRUE),
    Horas_SD = sqrt(wtd.var(horas_trab,peso, na.rm = TRUE)),
    Moradores_Mean = wtd.mean(pessoas,peso, na.rm = TRUE),
    Moradores_SD = sqrt(wtd.var(pessoas,peso, na.rm = TRUE)),
    Negro_Mean = wtd.mean(negro,peso, na.rm = TRUE),
    Negro_SD = sqrt(wtd.var(negro,peso, na.rm = TRUE)),
    Mulher_Mean = wtd.mean(fem,peso, na.rm = TRUE),
    Mulher_SD = sqrt(wtd.var(fem,peso, na.rm = TRUE)),
    Superior_Mean = wtd.mean(en_sup,peso,na.rm = TRUE),
    Superior_SD = sqrt(wtd.var(en_sup,peso,na.rm = TRUE)),
    Servic_Mean = wtd.mean(setor_atv_servic,peso,na.rm = TRUE),
    Servic_SD =  sqrt(wtd.var(setor_atv_servic,peso,na.rm = TRUE)),
    #
    Comerc_Mean = wtd.mean(setor_atv_comerc,peso,na.rm = TRUE),
    Comerc_SD =  sqrt(wtd.var(setor_atv_comerc,peso,na.rm = TRUE)),
    #
    Indust_Mean = wtd.mean(setor_atv_indust,peso,na.rm = TRUE),
    Indust_SD =  sqrt(wtd.var(setor_atv_indust,peso,na.rm = TRUE)),
    #
    Construc_Mean = wtd.mean(setor_atv_construc,peso,na.rm = TRUE),
    Construc_SD =  sqrt(wtd.var(setor_atv_construc,peso,na.rm = TRUE)),
    #
    Outros_Mean = wtd.mean(setor_atv_outros,peso,na.rm = TRUE),
    Outros_SD =  sqrt(wtd.var(setor_atv_outros,peso,na.rm = TRUE))
    #
  ) |>
  pivot_longer(cols = Ocupados_Mean:Outros_SD, names_to = "Variável", values_to = "Valor") |>
  separate(Variável, into = c("Variável", "Métrica")) |> 
  pivot_wider(names_from = c(grupo, Métrica), values_from = Valor)


tab1_gama

tabela_gama <- cbind(tab1_gama |> filter(periodo == "Pre-BRT") |> select(-periodo),
                tab1_gama |> filter(periodo == "Post-BRT")|> select(-c(periodo,Variável))) |> 
  kableExtra::kable(col.names = c("Variável", "Control Mean", "Control SD", "Treated Mean", "Treated SD", 
                                  "Control Mean", "Control SD", "Treated Mean", "Treated SD"),
                    align = "lcccclcccc",format = "latex") |>
  kableExtra::add_header_above(c(" " = 2, "Pre-BRT" = 3, "Post-BRT" = 4)) |>
  kableExtra::row_spec(0, bold = TRUE, font_size = 12) |>
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

tabela_gama

# base_gama Santa Maria ----

tab1_sm <- base_sm |>
  mutate(grupo = ifelse(grupo == 1,"Treated","Control"),
         periodo = ifelse(aamm < 201406, "Pre-BRT", "Post-BRT")) |>
  filter(periodo == "Pre-BRT") |> 
  group_by(grupo, periodo) |>
  summarise(
    Ocupados_Mean = wtd.mean(ocupado,peso, na.rm = TRUE),  
    Ocupados_SD = sqrt(wtd.var(ocupado,peso, na.rm = TRUE)),
    Informais_Mean = wtd.mean(informal,peso, na.rm = TRUE),
    Informais_SD = sqrt(wtd.var(informal,peso, na.rm = TRUE)),
    Rendimento_Mean = wtd.mean(rend_bruto,peso, na.rm = TRUE),
    Rendimento_SD = sqrt(wtd.var(rend_bruto,peso, na.rm = TRUE)),
    Horas_Mean = wtd.mean(horas_trab,peso, na.rm = TRUE),
    Horas_SD = sqrt(wtd.var(horas_trab,peso, na.rm = TRUE)),
    Moradores_Mean = wtd.mean(pessoas,peso, na.rm = TRUE),
    Moradores_SD = sqrt(wtd.var(pessoas,peso, na.rm = TRUE)),
    Negro_Mean = wtd.mean(negro,peso, na.rm = TRUE),
    Negro_SD = sqrt(wtd.var(negro,peso, na.rm = TRUE)),
    Mulher_Mean = wtd.mean(fem,peso, na.rm = TRUE),
    Mulher_SD = sqrt(wtd.var(fem,peso, na.rm = TRUE)),
    Superior_Mean = wtd.mean(en_sup,peso,na.rm = TRUE),
    Superior_SD = sqrt(wtd.var(en_sup,peso,na.rm = TRUE)),
    Servic_Mean = wtd.mean(setor_atv_servic,peso,na.rm = TRUE),
    Servic_SD =  sqrt(wtd.var(setor_atv_servic,peso,na.rm = TRUE)),
    #
    Comerc_Mean = wtd.mean(setor_atv_comerc,peso,na.rm = TRUE),
    Comerc_SD =  sqrt(wtd.var(setor_atv_comerc,peso,na.rm = TRUE)),
    #
    Indust_Mean = wtd.mean(setor_atv_indust,peso,na.rm = TRUE),
    Indust_SD =  sqrt(wtd.var(setor_atv_indust,peso,na.rm = TRUE)),
    #
    Construc_Mean = wtd.mean(setor_atv_construc,peso,na.rm = TRUE),
    Construc_SD =  sqrt(wtd.var(setor_atv_construc,peso,na.rm = TRUE)),
    #
    Outros_Mean = wtd.mean(setor_atv_outros,peso,na.rm = TRUE),
    Outros_SD =  sqrt(wtd.var(setor_atv_outros,peso,na.rm = TRUE))
    #
  ) |>
  pivot_longer(cols = Ocupados_Mean:Outros_SD, names_to = "Variável", values_to = "Valor") |>
  separate(Variável, into = c("Variável", "Métrica")) |> 
  pivot_wider(names_from = c(grupo, Métrica), values_from = Valor)

tab1_sm

tabela_sm <- cbind(tab1_sm |> filter(periodo == "Pre-BRT") |> select(-periodo),
                     tab1_sm |> filter(periodo == "Post-BRT")|>select(-c(periodo,Variável))) |> 
  kableExtra::kable(col.names = c("Variável", "Control Mean", "Control SD", "Treated Mean", "Treated SD", 
                                   "Control Mean", "Control SD", "Treated Mean", "Treated SD"),
                    align = "lcccclcccc",format = "latex") |>
  kableExtra::add_header_above(c(" " = 2, "Pre-BRT" = 3, "Post-BRT" = 4)) |>
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
