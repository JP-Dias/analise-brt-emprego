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

# Leitura da base_gama de dados
base <- readRDS("analysis/dados/base_ra.RDS") |> filter(reg %in% c("Gama","Santa Maria","Recanto Das Emas","Sobradinho"))

# Criação da Variável de Efeito ----

## Gama ----
dados_gama <- base |> filter(reg %in% c("Gama","Recanto Das Emas")) |> 
  mutate(grupo = ifelse(reg == "Gama",1,0),
         Effect = grupo * intervencao) |> # Montar Variável de Interesse
  filter(!is.na(ocupado)) 

# Santa Maria ----
dados_sm <- base |> filter(reg %in% c("Santa Maria","Sobradinho")) |>
  mutate(grupo = ifelse(reg == "Santa Maria",1,0),
         Effect = grupo * intervencao) |> # Montar Variável de Interesse
  filter(!is.na(ocupado))


# Estudo de Evento ----

## Gama ----
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
