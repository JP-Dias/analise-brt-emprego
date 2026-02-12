library("fixest")
library("dplyr")
library("HonestDiD")


#### Open Data ####

pasta <- "C:/Users/ricar/Desktop/Efeitos BRT/Dados e Estimativas/Results"

setwd(pasta)

dados <- readRDS("base_RA.RDS")

#### Constructing a Subset for Treated and Control Areas ###

dados3 <- subset(dados, reg %in% c("Santa Maria", "Recanto Das Emas") & !ano %in% c(2018, 2019))
dados3$BRT_Effect <- ifelse(dados3$aamm > "201406" & dados3$reg == "Santa Maria", 1, 0)
dados3$Treat <- ifelse(dados3$reg == "Santa Maria", 1, 0)

### Creating other Variables ###

dados3$horas_trabM <- dados3$horas_trab * 4

### Main Results - Santa Maria ###

attach(dados3)

reg3 <- feols(log(rend_bruto/horas_trabM) ~ BRT_Effect + idade | reg + aamm + fem + cor + escol + pessoas + setor_atv, ~conglom, data=dados3)
summary(reg3)

reg4 <- feols(log(rend_bruto/horas_trabM) ~ BRT_Effect + idade | reg + aamm + fem + cor + escol + pessoas + setor_atv, ~conglom, data=dados3)
summary(reg4, "hetero")

reg5 <- feols(log(rend_bruto/horas_trabM) ~ BRT_Effect + idade | reg + aamm + fem + cor + escol + pessoas + setor_atv, ~reg, data=dados3)
summary(reg5)

reg6 <- feols(log(rend_bruto/horas_trabM) ~ BRT_Effect + idade | reg + aamm + fem + cor + escol + pessoas + setor_atv, ~ aamm + conglom, data=dados3)
summary(reg6)

