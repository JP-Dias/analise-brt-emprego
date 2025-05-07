library("fixest")
library("dplyr")
library("HonestDiD")


#### Open Data ####

pasta <- "C:/Users/ricar/Desktop/Efeitos BRT/Dados e Estimativas"

setwd(pasta)

dados <- readRDS("base_RA.RDS")

### Create a Variable of Interest ###

dados$BRT_Effect <- ifelse(dados$aamm > "201406" & dados$reg == "Santa Maria", 1, 0)
dados$Treat <- ifelse(dados$reg == "Santa Maria", 1, 0)
dados$horas_trabM <- dados$horas_trab * 4


#### Constructing Different Control Groups ###

dados2 <- subset(dados, reg %in% c("Santa Maria", "Riacho Fundo") & !ano %in% c(2018, 2019))
dados3 <- subset(dados, reg %in% c("Santa Maria", "Ceilândia") & !ano %in% c(2018, 2019))
dados4 <- subset(dados, reg %in% c("Santa Maria", "Paranoá") & !ano %in% c(2018, 2019))
dados5 <- subset(dados, reg %in% c("Santa Maria", "Planaltina") & !ano %in% c(2018, 2019))


### Robustness - Different Control Groups ####

reg1 <- feols(log(rend_bruto/horas_trabM) ~ BRT_Effect + idade  | reg + aamm + fem + cor + escol + pessoas + setor_atv, ~conglom, data=dados2)
summary(reg1)

reg2 <- feols(log(rend_bruto/horas_trabM) ~ BRT_Effect + idade | reg + aamm + fem + cor + escol + pessoas + setor_atv, ~conglom, data=dados3)
summary(reg2)

reg3 <- feols(log(rend_bruto/horas_trabM) ~ BRT_Effect + idade | reg + aamm + fem + cor + escol  + pessoas + setor_atv, ~conglom, data=dados4)
summary(reg3)

reg4 <- feols(log(rend_bruto/horas_trabM) ~ BRT_Effect + idade  | reg + aamm + fem + cor + escol + pessoas + setor_atv, ~conglom, data=dados5)
summary(reg4)

