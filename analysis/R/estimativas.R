library("fixest")
library("dplyr")


#### Abrir Dados ####

pasta <- "C:/Users/ricar/Desktop/Efeitos BRT"

setwd(pasta)

dados <- readRDS("base.RDS")

#### Montar Variável de Interesse ###

dados <- dados[!is.na(dados$ocupado), ]

attach(dados)

dados$Effect <- grupo_20 * intervencao

#### Estimação Principal ####

reg1 <- feols(trab_plano ~ Effect | CODSETOR2000 + ano, ~CODSETOR2000, data=dados)
summary(reg1, se="twoway")
summary(reg1)

#### Event-Study TWFE ####

reg2 <- feols(trab_plano ~ i(ano, grupo_20, 2013)| CODSETOR2000 + ano, 
              data=dados)
summary(reg2, se="twoway")

iplot(reg2, se="twoway")
