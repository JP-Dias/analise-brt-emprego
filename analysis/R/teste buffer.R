# Pacotes
library(tidyverse)
library(plm)
library(stargazer)
library(lubridate)
library(sidrar)
library(fixest)
library(zoo)
library(stargazer)
library(modelsummary)

source("config.R")

# Leitura da base_gama de dados
dados_15 <- readRDS("analysis/dados/base_buffer_ra_15.RDS") 
dados_20 <- readRDS("analysis/dados/base_buffer_ra_20.RDS") 
dados_35 <- readRDS("analysis/dados/base_buffer_ra_35.RDS") 
dados_45 <- readRDS("analysis/dados/base_buffer_ra_45.RDS") 
dados_55 <- readRDS("analysis/dados/base_buffer_ra_55.RDS") 
dados_65 <- readRDS("analysis/dados/base_buffer_ra_65.RDS") 

dados <- readRDS("analysis/dados/base_ra.RDS")



dados2 <- subset(dados, reg %in% c("Gama", "Brazlândia"))
dados2 <- subset(dados2, !ano %in% c(2009, 2018, 2019))

dados3 <- subset(dados, reg %in% c("Santa Maria", "Recanto Das Emas"))
dados3 <- subset(dados3, !ano %in% c(2009, 2018, 2019))

dados3_15 <- subset(dados_15, reg %in% c("Santa Maria", "Recanto Das Emas"))
dados3_20 <- subset(dados_20, reg %in% c("Santa Maria", "Recanto Das Emas"))
dados3_35 <- subset(dados_35, reg %in% c("Santa Maria", "Recanto Das Emas"))
dados3_45 <- subset(dados_45, reg %in% c("Santa Maria", "Recanto Das Emas"))
dados3_55 <- subset(dados_55, reg %in% c("Santa Maria", "Recanto Das Emas"))
dados3_65 <- subset(dados_65, reg %in% c("Santa Maria", "Recanto Das Emas"))

dados3_15 <- subset(dados3_15, !ano %in% c(2009, 2018, 2019))
dados3_20 <- subset(dados3_20, !ano %in% c(2009, 2018, 2019))
dados3_35 <- subset(dados3_35, !ano %in% c(2009, 2018, 2019))
dados3_45 <- subset(dados3_45, !ano %in% c(2009, 2018, 2019))
dados3_55 <- subset(dados3_55, !ano %in% c(2009, 2018, 2019))
dados3_65 <- subset(dados3_65, !ano %in% c(2009, 2018, 2019))


### Create a Variable of Interest ###

dados3$BRT_Effect <- ifelse(dados3$aamm > "201406" & dados3$reg == "Santa Maria", 1, 0)
dados3$Treat <- ifelse(dados3$reg == "Santa Maria", 1, 0)

dados3_15$BRT_Effect <- ifelse(dados3_15$aamm > "201406" & dados3_15$reg == "Santa Maria", 1, 0)
dados3_20$BRT_Effect <- ifelse(dados3_20$aamm > "201406" & dados3_20$reg == "Santa Maria", 1, 0)
dados3_35$BRT_Effect <- ifelse(dados3_35$aamm > "201406" & dados3_35$reg == "Santa Maria", 1, 0)
dados3_45$BRT_Effect <- ifelse(dados3_45$aamm > "201406" & dados3_45$reg == "Santa Maria", 1, 0)
dados3_55$BRT_Effect <- ifelse(dados3_55$aamm > "201406" & dados3_55$reg == "Santa Maria", 1, 0)
dados3_65$BRT_Effect <- ifelse(dados3_65$aamm > "201406" & dados3_65$reg == "Santa Maria", 1, 0)


dados3_15$Treat <- ifelse(dados3_15$reg == "Santa Maria", 1, 0)
dados3_20$Treat <- ifelse(dados3_20$reg == "Santa Maria", 1, 0)
dados3_35$Treat <- ifelse(dados3_35$reg == "Santa Maria", 1, 0)
dados3_45$Treat <- ifelse(dados3_45$reg == "Santa Maria", 1, 0)
dados3_55$Treat <- ifelse(dados3_55$reg == "Santa Maria", 1, 0)
dados3_65$Treat <- ifelse(dados3_65$reg == "Santa Maria", 1, 0)


dados2$BRT_Effect <- ifelse(dados2$aamm > "201406" & dados2$reg == "Gama", 1, 0)
dados2$Treat <- ifelse(dados2$reg == "Gama", 1, 0)




#### Main Results - Santa Maria ####

#attach(dados3)

reg1 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 + Treat |  aamm + fem + cor + escol + posicao_fam + setor_atv, ~conglom,
              weights = ~peso, data=dados3)

reg2 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 + Treat |  aamm + fem + cor + escol + posicao_fam + setor_atv, ~conglom,
              weights = ~peso, data=dados3_15)

reg3 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 + Treat |  aamm + fem + cor + escol + posicao_fam + setor_atv, ~conglom,
              weights = ~peso, data=dados3_20)

reg4 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 + Treat|  aamm + fem + cor + escol + posicao_fam + setor_atv, ~conglom,
              weights = ~peso, data=dados3_35)

reg5 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 + Treat |  aamm + fem + cor + escol + posicao_fam + setor_atv, ~conglom,
              weights = ~peso, data=dados3_45)

reg6 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 + Treat |  aamm + fem + cor + escol + posicao_fam + setor_atv, ~conglom,
              weights = ~peso, data=dados3_55)

reg7 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 + Treat |  aamm + fem + cor + escol + posicao_fam + setor_atv, ~conglom,
              weights = ~peso, data=dados3_65)

# --------------------
reg1 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 + Treat |  aamm + fem + cor + escol + posicao_fam + setor_atv, ~reg,
              weights = ~peso, data=dados3)

reg2 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 + Treat |  aamm + fem + cor + escol + posicao_fam + setor_atv, ~reg,
              weights = ~peso, data=dados3_15)

reg3 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 + Treat |  aamm + fem + cor + escol + posicao_fam + setor_atv, ~reg,
              weights = ~peso, data=dados3_20)

reg4 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 + Treat|  aamm + fem + cor + escol + posicao_fam + setor_atv, ~reg,
              weights = ~peso, data=dados3_35)

reg5 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 + Treat |  aamm + fem + cor + escol + posicao_fam + setor_atv, ~reg,
              weights = ~peso, data=dados3_45)

reg6 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 + Treat |  aamm + fem + cor + escol + posicao_fam + setor_atv, ~reg,
              weights = ~peso, data=dados3_55)

reg7 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 + Treat |  aamm + fem + cor + escol + posicao_fam + setor_atv, ~reg,
              weights = ~peso, data=dados3_65)

# --------------------


modelsummary(list("Área Total" = reg1,
                  "Raio 2,0km" = reg3,
                  "Raio 3,5km" = reg4,
                  "Raio 4,5km" = reg5,
                  "Raio 5,5km" = reg6,
                  "Raio 6,5km" = reg7
                  ),
             coef_map = c("BRT_Effect" = "Efeito BRT"),
             output = "latex_tabular",
             stars = T)



reg1 <- feols(log(rend_bruto) ~ BRT_Effect | reg + aamm, ~reg,data=dados2)
summary(reg1)

reg2 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg + aamm + fem + cor + posicao_fam, ~reg,weights = ~peso, data=dados2)
summary(reg2)

reg3 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg + aamm + fem + cor + escol + posicao_fam + setor_atv, ~reg,weights = ~peso, data=dados2)
summary(reg3)

reg4 <- feols(log(rend_bruto) ~ BRT_Effect | reg + aamm, ~conglom,weights = ~peso,data=dados2)
summary(reg4)

reg5 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg + aamm + fem + cor + posicao_fam, ~conglom,weights = ~peso, data=dados2)
summary(reg5)

reg6 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg + aamm + fem + cor + escol + posicao_fam + setor_atv, ~conglom,weights = ~peso, data=dados2)
summary(reg6)


modelsummary(list(reg1,reg2,reg3,reg4,reg5,reg6
),coef_map = c("BRT_Effect" = "Efeito BRT"),
#output = "latex_tabular",
stars = T)
