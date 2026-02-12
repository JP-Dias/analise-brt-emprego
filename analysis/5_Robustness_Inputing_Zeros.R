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
dados3$horas_trabM <- dados3$horas_trab * 4

### Imputar Zero de Renda para os Desempregados ###

dados3 <- dados3 %>%
  filter(!is.na(ocupado))

dados3 <- dados3 %>%
  mutate(
    log_renda_hora = log(rend_bruto / horas_trabM),
    log_renda_hora = coalesce(log_renda_hora, 0)
  )

### Main Results - Santa Maria ###

reg1 <- feols(log_renda_hora ~ BRT_Effect | reg + aamm, ~conglom, data=dados3)
summary(reg1)

reg2 <- feols(log_renda_hora~ BRT_Effect + idade  | reg + aamm + fem + cor + pessoas + escol, ~conglom, data=dados3)
summary(reg2)





