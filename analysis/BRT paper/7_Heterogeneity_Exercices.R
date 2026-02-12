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
dados3$horas_trabM <- dados3$horas_trab * 4

#### Heterogeneous Effects ######

dados3$dummy_preto <- ifelse(dados3$cor %in% c("preta", "parda"), 1, 0)
dados3$dummy_low_skill <- ifelse(dados3$escol %in% c("analf", "fund_inc", "fund_com", "med_inc"), 1, 0)


reg1 <- feols(log(rend_bruto/horas_trabM) ~ dummy_preto*BRT_Effect | reg + aamm , ~conglom, data=dados3)
summary(reg1)

reg2 <- feols(log(rend_bruto/horas_trabM) ~ BRT_Effect + idade + dummy_preto:BRT_Effect | reg + 
                aamm + fem + cor + escol + pessoas + setor_atv, ~conglom, data=dados3)
summary(reg2)

reg3 <- feols(log(rend_bruto/horas_trabM) ~ fem*BRT_Effect | reg + aamm , ~conglom, data=dados3)
summary(reg3)

reg4 <- feols(log(rend_bruto/horas_trabM) ~ BRT_Effect + idade + fem:BRT_Effect | reg + 
                aamm + fem + cor + escol + pessoas + setor_atv, ~conglom, data=dados3)
summary(reg4)

reg5 <- feols(log(rend_bruto/horas_trabM) ~ dummy_low_skill*BRT_Effect | reg + aamm , ~conglom, data=dados3)
summary(reg5)

reg6 <- feols(log(rend_bruto/horas_trabM) ~ BRT_Effect + idade + dummy_low_skill:BRT_Effect | reg + 
                aamm + fem + cor + escol + pessoas + setor_atv, ~conglom, data=dados3)
summary(reg6)



