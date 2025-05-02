library("fixest")
library("dplyr")
library("HonestDiD")


#### Open Data ####

pasta <- "C:/Users/ricar/Desktop/Efeitos BRT/Dados e Estimativas"

setwd(pasta)

dados <- readRDS("base_RA.RDS")

#### Constructing a Subset for Treated and Control Areas ###

dados3 <- subset(dados, reg %in% c("Santa Maria", "Recanto Das Emas") & !ano %in% c(2018, 2019))

### Create a Variable of Interest ###

dados3$BRT_Effect <- ifelse(dados3$aamm > "201406" & dados3$reg == "Santa Maria", 1, 0)
dados3$Treat <- ifelse(dados3$reg == "Santa Maria", 1, 0)

### Creating other Variables ###

dados3$horas_trabM <- dados3$horas_trab * 4
dados3$rend_bruto2<- ifelse(dados3$ocupado == 0, 0, dados3$ocupado)

#### Different Definitions of the Outcome Variable ####

reg1 <- feols(log(rend_bruto2/horas_trabM) ~ BRT_Effect + idade | reg + aamm + fem + cor + escol + pessoas + setor_atv, ~conglom, data=dados3)
summary(reg1)

reg2 <- feols(log(rend_bruto) ~ BRT_Effect + idade | reg + aamm + fem + cor + escol + pessoas + setor_atv, ~conglom, data=dados3)
summary(reg2)

reg3 <- feols(rend_bruto ~ BRT_Effect + idade | reg + aamm + fem + cor + escol + pessoas + setor_atv, ~conglom, data=dados3)
summary(reg3)

reg4 <- feols(rend_bruto/horas_trabM ~ BRT_Effect + idade | reg + aamm + fem + cor + escol + pessoas + setor_atv, ~conglom, data=dados3)
summary(reg4)

reg5 <- feols(log(rend_bruto/pessoas) ~ BRT_Effect + idade | reg + aamm + fem + cor + escol + pessoas + setor_atv, ~conglom, data=dados3)
summary(reg5)
