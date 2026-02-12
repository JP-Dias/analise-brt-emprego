library("fixest")
library("dplyr")
library("HonestDiD")


#### Open Data ####

pasta <- "C:/Users/ricar/Desktop/Efeitos BRT/Dados e Estimativas/Results"

setwd(pasta)

dados <- readRDS("base_RA.RDS")

#### Constructing a Subset for Treated and Control Areas ###

dados3 <- subset(dados, reg %in% c("Santa Maria", "Recanto Das Emas") & !ano %in% c(2018, 2019))

### Create a Variable of Interest ###

dados3$BRT_Effect <- ifelse(dados3$aamm > "201406" & dados3$reg == "Santa Maria", 1, 0)
dados3$Treat <- ifelse(dados3$reg == "Santa Maria", 1, 0)

### Repeting the Sample of Main Results ###

attach(dados3)

dados3$horas_trabM <- dados3$horas_trab * 4
dados3$rendimento <- dados3$rend_bruto/horas_trabM
dados3<- subset(dados3, rendimento!= 0 & !is.na(rendimento))

### Mechanism 1 ####

reg1 <- feols(trab_plano ~ BRT_Effect | reg + aamm, ~conglom, data=dados3)
summary(reg1)

reg2 <- feols(trab_plano ~ BRT_Effect + idade  | reg + aamm + fem + cor + escol + pessoas + setor_atv, ~conglom, data=dados3)
summary(reg2)

reg3 <- feols(informal ~ BRT_Effect | reg + aamm, ~conglom, data=dados3)
summary(reg3)

reg4 <- feols(informal ~ BRT_Effect + idade  | reg + aamm + fem + cor + escol + pessoas + setor_atv, ~conglom, data=dados3)
summary(reg4)

