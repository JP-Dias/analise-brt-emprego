library("fixest")
library("dplyr")
library("devtools")
library("synthdid")
library("zoo")

#### Open Data ####

pasta <- "C:/Users/ricar/Desktop/Efeitos BRT/Dados e Estimativas/Results"

setwd(pasta)


### Synth DiD ###

dados <- readRDS("dados_synth_did.rds")

### Converter em Mês-Ano ###

dados$aamm <- as.yearmon(as.character(dados$aamm), "%Y%m")

dados_ago <- subset(dados, format(aamm, "%m") == "09")
dados_ago$aamm <- as.numeric(format(dados_ago$aamm, "%Y"))


#### Limpando Painel ###

dados2 <- subset(dados_ago, !(reg %in% c("Brasília", "Plano Piloto", "Lago Sul", "Candangolândia",
                                     "Lago Norte","Riacho Fundo")| is.na(reg)))


#### Synth DiD ####

dados3 <- panel.matrices(
  dados2,
  unit = 1,
  time = 2,
  outcome = 3,
  treatment = 4,
  treated.last = TRUE
)




