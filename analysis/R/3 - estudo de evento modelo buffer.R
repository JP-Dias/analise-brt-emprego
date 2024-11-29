# Pacotes ----
library(tidyverse)
library(plm)
library(fixest)
library(stargazer)

dados <- readRDS("analysis/dados/base.RDS")

# Montar Variável de Interesse
dados <- dados[!is.na(dados$ocupado), ]

attach(dados)

dados$Effect <- grupo_20 * intervencao

#Event-Study Two Way Fixed-Effects

# Setor
iplot(feols(ocupado ~ i(ano, grupo_20, 2013)| setor + ano, data=dados), se="twoway",col = "darkblue", sub = "Efeitos fixos setor e ano")
iplot(feols(informal ~ i(ano, grupo_20, 2013)| setor + ano, data=dados), se="twoway",col = "darkblue", sub = "Efeitos fixos setor e ano")
iplot(feols(ln_rend_bruto ~ i(ano, grupo_20, 2013)| setor + ano, data=dados), se="twoway",col = "darkblue", sub = "Efeitos fixos setor e ano")
iplot(feols(ln_horas_trab ~ i(ano, grupo_20, 2013)| setor + ano, data=dados), se="twoway",col = "darkblue", sub = "Efeitos fixos setor e ano")
iplot(feols(trab_plano ~ i(ano, grupo_20, 2013)| setor + ano, data=dados), se="twoway",col = "darkblue", sub = "Efeitos fixos setor e ano")

# Bairro
iplot(feols(ocupado ~ i(ano, grupo_20, 2013)| bairro + ano, data=dados), se="twoway",col = "darkblue", sub = "Efeitos fixos bairro e ano")
iplot(feols(informal ~ i(ano, grupo_20, 2013)| bairro + ano, data=dados), se="twoway",col = "darkblue", sub = "Efeitos fixos bairro e ano")
iplot(feols(ln_rend_bruto ~ i(ano, grupo_20, 2013)| bairro + ano, data=dados), se="twoway",col = "darkblue", sub = "Efeitos fixos bairro e ano")
iplot(feols(ln_horas_trab ~ i(ano, grupo_20, 2013)| bairro + ano, data=dados), se="twoway",col = "darkblue", sub = "Efeitos fixos bairro e ano")
iplot(feols(trab_plano ~ i(ano, grupo_20, 2013)| bairro + ano, data=dados), se="twoway",col = "darkblue", sub = "Efeitos fixos bairro e ano")

