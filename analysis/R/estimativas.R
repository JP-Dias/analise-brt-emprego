# Pacotes ----
library(tidyverse)
library(plm)
library(fixest)
library(stargazer)

dados <- readRDS("analysis/dados/base.RDS")

#### Montar Variável de Interesse ###

dados <- dados[!is.na(dados$ocupado), ]

attach(dados)

dados$Effect <- grupo_20 * intervencao

#### Estimação Principal ####

reg1 <- feols(trab_plano ~ Effect | CODSETOR2000 + ano, ~CODSETOR2000, 
              data=dados)
summary(reg1, se="twoway")
summary(reg1)

modelsummary(list(reg1, reg2))


#### Event-Study TWFE ####

reg2 <- feols(trab_plano ~ i(ano, grupo_20, 2013)| CODSETOR2000 + ano, 
              data=dados)
summary(reg2, se="twoway")

iplot(reg2, se="twoway")


reg2 <- feols(rend_bruto ~ i(ano, grupo_20, 2013)| CODSETOR2000 + ano, 
              data=dados)
summary(reg2, se="twoway")

iplot(reg2, se="twoway",col = "darkblue", sub = "Efeitos fixos setor e ano")

