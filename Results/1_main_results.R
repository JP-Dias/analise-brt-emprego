library("fixest")
library("dplyr")
library("HonestDiD")


#### Open Data ####

pasta <- "C:/Users/ricar/Desktop/Efeitos BRT/Dados e Estimativas/Results"

setwd(pasta)

dados <- readRDS("base_RA.RDS")

#### Constructing a Subset for Treated and Control Areas ###

dados2 <- subset(dados, reg %in% c("Gama", "Sobradinho") & !ano %in% c(2018, 2019))
dados3 <- subset(dados, reg %in% c("Santa Maria", "Recanto Das Emas") & !ano %in% c(2018, 2019))

### Create a Variable of Interest ###

dados2$BRT_Effect <- ifelse(dados2$aamm > "201406" & dados2$reg == "Gama", 1, 0)
dados2$Treat <- ifelse(dados2$reg == "Gama", 1, 0)

dados3$BRT_Effect <- ifelse(dados3$aamm > "201406" & dados3$reg == "Santa Maria", 1, 0)
dados3$Treat <- ifelse(dados3$reg == "Santa Maria", 1, 0)

### Creating other Variables ###

dados2$horas_trabM <- dados2$horas_trab * 4
dados3$horas_trabM <- dados3$horas_trab * 4

### Main Results - Santa Maria ###

attach(dados3)

reg1 <- feols(log(rend_bruto/horas_trabM) ~ BRT_Effect | reg + aamm, ~conglom, data=dados3)
summary(reg1)

reg2 <- feols(log(rend_bruto/horas_trabM) ~ BRT_Effect + idade  | reg + aamm + fem + cor + pessoas + escol, ~conglom, data=dados3)
summary(reg2)

reg3 <- feols(log(rend_bruto/horas_trabM) ~ BRT_Effect + idade | reg + aamm + fem + cor + escol + pessoas + setor_atv, ~conglom, data=dados3)
summary(reg3)

reg4 <- feols(ocupado ~ BRT_Effect | reg + aamm, ~conglom, data=dados3)
summary(reg4)

reg5 <- feols(ocupado ~ BRT_Effect + idade  | reg + aamm + fem + cor + pessoas + escol, ~conglom, data=dados3)
summary(reg5)

### Main Results - Gama ####

attach(dados2)

reg1 <- feols(log(rend_bruto/horas_trabM) ~ BRT_Effect | reg + aamm, ~conglom, data=dados2)
summary(reg1)

reg2 <- feols(log(rend_bruto/horas_trabM) ~ BRT_Effect + idade | reg + aamm + fem + cor + pessoas + escol, ~conglom, data=dados2)
summary(reg2)

reg3 <- feols(log(rend_bruto/horas_trabM) ~ BRT_Effect + idade  | reg + aamm + fem + cor + escol + pessoas + setor_atv, ~conglom, data=dados2)
summary(reg3)

reg4 <- feols(ocupado ~ BRT_Effect | reg + aamm, ~conglom, data=dados2)
summary(reg4)

reg5 <- feols(ocupado ~ BRT_Effect + idade | reg + aamm + fem + cor + posicao_fam + escol, ~conglom, data=dados2)
summary(reg5)









