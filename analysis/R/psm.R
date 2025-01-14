library("fixest")
library("dplyr")
library("MatchIt")

source("config.R")

options(scipen=99999)

`%notin%` <- negate(`%in%`)

dados <- readRDS(analysis("dados/dados_controle_sintetico.RDS"))


dados2 <- subset(dados, ano %in% c(2013)) 

dados2 <- fastDummies::dummy_cols(dados2,"ano")

dados3 <- subset(dados2, reg %notin% c("Santa Maria","Sobradinho","Plano Piloto","Lago Norte","Lago Sul","Cruzeiro"))

dados3 <- dados3 |> mutate(Gama_Treat=ifelse(reg == "Gama", 1, 0))

PSM <- matchit(Gama_Treat ~ perc_analf + perc_idoso + perc_informal + perc_mora_trabalha + 
                 perc_sup + media_familia + media_idade + perc_nasc_df + 
                 #ano_2010 + ano_2011 + ano_2012 +
                 log(renda_fam_media) + log(n_empregos) + log(massa_salarial) + log(densidade_2010), 
               data = dados3, 
               method = "nearest",
               link = "logit")
PSM
match.data(PSM)

m.data1 <- match.data(PSM)

m.data1

summary(PSM) |> plot()

##### Fixar Antes do Choque (2013) - PSM de Santa Maria #####l

dados2 <- subset(dados, ano %in% c(2009)) 

dados4 <- subset(dados2, dados2$reg %notin% c("Gama","Sobradinho","Plano Piloto","Lago Norte","Lago Sul","Cruzeiro"))

dados4$Santa_Maria_Treat <- ifelse(dados4$reg == "Santa Maria", 1, 0)


PSM2 <- matchit(Santa_Maria_Treat ~ perc_analf + perc_idoso + perc_informal + perc_mora_trabalha
                + log(n_empregos) + log(massa_salarial) + log(densidade_2010), data = dados4, method = "nearest",link = "logit")

PSM2
summary(PSM2)
match.data(PSM2)

m.data2 <- match.data(PSM2)

m.data2