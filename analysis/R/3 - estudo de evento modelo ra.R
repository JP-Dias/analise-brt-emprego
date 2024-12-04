# Pacotes
library(tidyverse)
library(plm)
library(stargazer)
library(lubridate)
library(sidrar)
library(fixest)
library(zoo)
library(stargazer)
library(modelsummary)
library(fastDummies)

# Leitura da base_gama de dados
base <- readRDS("analysis/dados/base_ra.RDS") |> filter(reg %in% c("Gama","Santa Maria","Recanto Das Emas","Sobradinho"))

# Criação da Variável de Efeito ----

## Gama ----
dados_gama <- base |> filter(reg %in% c("Gama","Sobradinho")) |> 
  mutate(grupo = ifelse(reg == "Gama",1,0),
         Effect = grupo * intervencao) |> # Montar Variável de Interesse
  filter(!is.na(ocupado), idade %in% c(18:64), mora_mesma_ra == 1) |> 
  mutate(painel = case_when(mes %in% c(1,4,7,10)~"A",
                            mes %in% c(2,5,8,11)~"B",
                            TRUE~"C"))


# Santa Maria ----
dados_sm <- base |> filter(reg %in% c("Santa Maria","Recanto Das Emas")) |>
  mutate(grupo = ifelse(reg == "Santa Maria",1,0),
         Effect = grupo * intervencao) |> # Montar Variável de Interesse
  filter(!is.na(ocupado), idade %in% c(18:64), mora_mesma_ra == 1) |> 
  mutate(painel = case_when(mes %in% c(1,4,7,10)~"A",
                            mes %in% c(2,5,8,11)~"B",
                            TRUE~"C"))


# Estudo de Evento ----

vars <- as.formula("informal~i(ano, grupo, 2013) + grupo + escol_fund_com + escol_med_com + escol_sup_com + fem + negro")


iplot(feols(vars , data = teste), 
      se ="cluster",
      col = "darkblue", 
      sub = "Efeitos fixos reg e ano mes",
      main = "Teste")


feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo + en_sup + fem + negro | mes, cluster = ~conglom,data = dados_gama)



## Gama ----
iplot(feols(ocupado ~ i(ano, grupo, 2013) + grupo + en_sup + fem | aamm, data=dados_gama), 
      se ="cluster",
      col = "darkblue", 
      sub = "Efeitos fixos reg e ano mes",
      main = "feols(ocupado ~ i(ano, grupo, 2013) + grupo + en_sup + fem | aamm, data=dados_gama)")

iplot(feols(ocupado ~ i(ano, grupo, 2013) + grupo + en_sup + fem + negro + idade + idade2| aamm, data=dados_gama), 
      se ="cluster",
      col = "darkblue", 
      sub = "Efeitos fixos reg e ano mes",
      main = "feols(ocupado ~ i(ano, grupo, 2013) + grupo + en_sup + fem + negro + idade + idade2| aamm, data=dados_gama)")

iplot(feols(ocupado ~ i(ano, grupo, 2013) + grupo + fem + negro + idade + idade2 | mes, data=dados_gama, cluster = ~mes), 
      se ="cluster",
      col = "darkblue", 
      sub = "Efeitos fixos reg e ano mes",
      main = "feols(ocupado ~ i(ano, grupo, 2013) + grupo| aamm, data=dados_gama)")

iplot(feols(informal ~ i(ano, grupo, 2013) + en_sup + fem + negro + idade + idade2 | grupo, data=dados_gama |> filter(trab_plano == 1)), 
      se ="cluster",
      col = "darkblue", sub = "Efeitos fixos reg e ano mes",
      main = "feols(informal ~ i(ano, grupo, 2013) +  grupo + en_sup + fem + negro + idade + idade2 | aamm, data=dados_gama |> filter(trab_plano == 1))")

iplot(feols(ln_rend_bruto ~ i(ano, grupo, 2013) + grupo + en_sup + fem | aamm, data=dados_gama |> filter(trab_plano == 1)), 
      se ="cluster",
      col = "darkblue", sub = "Efeitos fixos reg e ano mes",
      main = "feols(ln_rend_bruto ~ i(ano, grupo, 2013) + grupo + en_sup + fem | aamm, data=dados_gama |> filter(trab_plano == 1))")

iplot(feols(ln_horas_trab ~ i(ano, grupo, 2013) + grupo + en_sup + fem | aamm, data=dados_gama |> filter(trab_plano == 1)),
      se ="cluster",
      col = "darkblue", 
      sub = "Efeitos fixos reg e ano mes",
      main = "feols(ln_horas_trab ~ i(ano, grupo, 2013) + grupo + en_sup + fem | aamm, data=dados_gama |> filter(trab_plano == 1))")

iplot(feols(trab_plano ~ i(ano, grupo, 2013) + grupo + en_sup + fem | aamm, data=dados_gama), 
      se ="cluster",
      col = "darkblue", 
      sub = "Efeitos fixos reg e ano mes",
      main = "feols(trab_plano ~ i(ano, grupo, 2013) + grupo + en_sup + fem | aamm, data=dados_gama)")


iplot(feols(ocupado ~ i(ano, grupo, 2013)| reg + ano, data=dados_gama), se="twoway",col = "darkblue", sub = "Efeitos fixos reg e ano")
iplot(feols(informal ~ i(ano, grupo, 2013)| reg + ano, data=dados_gama), se="twoway",col = "darkblue", sub = "Efeitos fixos reg e ano")
iplot(feols(ln_rend_bruto ~ i(ano, grupo, 2013)| reg + ano, data=dados_gama), se="twoway",col = "darkblue", sub = "Efeitos fixos reg e ano")
iplot(feols(ln_horas_trab ~ i(ano, grupo, 2013)| reg + ano, data=dados_gama), se="twoway",col = "darkblue", sub = "Efeitos fixos reg e ano")
iplot(feols(trab_plano ~ i(ano, grupo, 2013)| reg + ano, data=dados_gama), se="twoway",col = "darkblue", sub = "Efeitos fixos reg e ano")


## Santa Maria ----
iplot(feols(ocupado ~ i(ano, grupo, 2013) | reg + ano, data=dados_sm), se="twoway",ci_level = .95,col = "darkblue", sub = "Efeitos fixos reg e ano") 
iplot(feols(informal ~ i(ano, grupo, 2013)| reg + ano, data=dados_sm), se="twoway",col = "darkblue", sub = "Efeitos fixos reg e ano")
iplot(feols(ln_rend_bruto ~ i(ano, grupo, 2013)| reg + ano, data=dados_sm), se="twoway",col = "darkblue", sub = "Efeitos fixos reg e ano")
iplot(feols(ln_horas_trab ~ i(ano, grupo, 2013)| reg + ano, data=dados_sm), se="twoway",col = "darkblue", sub = "Efeitos fixos reg e ano")
iplot(feols(trab_plano ~ i(ano, grupo, 2013)| reg + ano, data=dados_sm), se="twoway",col = "darkblue", sub = "Efeitos fixos reg e ano")

as.ggplot(iplot(feols(ocupado ~ i(ano, grupo, 2013) | reg + ano, data=dados_sm), se="twoway",ci_level = .95,col = "darkblue", sub = "Efeitos fixos reg e ano") )
