library("fixest")
library("dplyr")
library("HonestDiD")


#### Open Data ####

pasta <- "C:/Users/ricar/Desktop/Efeitos BRT/Dados e Estimativas/Results"

setwd(pasta)

dados <- readRDS("dados_agg_reg.rds")


### Creating Variable of Interest ####

dados$BRT_Effect <- ifelse(dados$aamm > "201406" & dados$reg == "Santa Maria", 1, 0)
dados$log_Rend_Horas <- log(dados$rend_bruto/dados$horas_trab)
dados$log_Rend_pc <- log(dados$renda_pc + 1)


### Estimating Compositional Effects ###

reg1 <- feols(prop_fem ~ BRT_Effect | reg + aamm, data=dados)
summary(reg1, "hetero")

reg2 <- feols(prop_preta_parda ~ BRT_Effect | reg + aamm, data=dados)
summary(reg2, "hetero")

reg3 <- feols(prop_fam_3plus ~ BRT_Effect | reg + aamm, data=dados)
summary(reg3, "hetero")

reg4 <- feols(prop_analf ~ BRT_Effect | reg + aamm, data=dados)
summary(reg4, "hetero")

reg5 <- feols(prop_setor_serv_comerc ~ BRT_Effect | reg + aamm, data=dados)
summary(reg5, "hetero")
summary(reg5)

reg6 <- feols(prop_setor_setor_ind ~ BRT_Effect | reg + aamm, data=dados)
summary(reg6, "hetero")
summary(reg6)

reg7 <- feols(renda_pc ~ BRT_Effect | reg + aamm, data=dados)
summary(reg7, "hetero")

reg7 <- feols(log_Rend_Horas ~ BRT_Effect | reg + aamm, data=dados)
summary(reg7, "hetero")

dados2 <- subset(dados, select = c(reg, aamm, log_Rend_pc, BRT_Effect))
saveRDS(dados2, file = "dados_synth_did.rds")


