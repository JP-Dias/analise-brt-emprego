# Pacotes
library(tidyverse)
library(plm)
library(stargazer)

base <- readRDS("analysis/dados/base.RDS")

# Criando o data frame com os dados do PIB
pib <- data.frame(
  ano = as.character(2003:2021),
  pib_br = c(1.1, 5.8, 3.2, 4.0, 6.1, 5.1, -0.1, 7.5, 4.0, 1.9, 3.0, 0.5, -3.5, -3.3, 1.3, 1.8, 1.2, -3.3, 4.8),
  pib_df = c(0.7, 5.0, 5.8, 5.5, 6.6, 4.5, 5.0, 4.4, 3.7, 0.8, 3.7, 2.0, -1.0, 0.0, 0.3, 1.7, 2.1,-2.6,3.0)
)

data <- base |> 
  filter(!is.na(ocupado),
         !is.na(escol_sup_com)) |> 
  mutate(
    intervencao_14_15 = ifelse(ano %in% c("2014","2015"),1,0),
    intervencao_15_17 = ifelse(ano %in% c("2015","2016","2017"),1,0),
    intervencao_17_19 = ifelse(ano %in% c("2017","2018","2019"),1,0),
         ) |> 
  select(ocupado,informal,ln_rend_bruto,ln_horas_trab,
         setor = CODSETOR2000,
         intervencao, 
         intervencao_14_15,
         intervencao_15_17,
         intervencao_17_19,
         trat20 = grupo_20,trat30 = grupo_30,
         ano,mes,ra = NM_SUBDIST, mora_mesma_ra,escol_sup_com,
         idade, escol_sup_com, fem, pessoas) |> 
  left_join(pib)


m1 <- lm(formula = ocupado ~ trat20 + intervencao_14_15 + intervencao_15_17 + intervencao_17_19 + 
                   (trat20 * intervencao_14_15) + (trat20 * intervencao_15_17) + (trat20 * intervencao_17_19)+ pib_df,
                 data = data) 


m2 <- lm(formula = ocupado ~ trat20 + intervencao + (trat20 * intervencao)+ pib_df,
         data = data) 

m3 <- lm(formula = informal ~ trat20 + intervencao_14_15 + intervencao_15_17 + intervencao_17_19 + 
           (trat20 * intervencao_14_15) + (trat20 * intervencao_15_17) + (trat20 * intervencao_17_19) + pib_df,
         data = data) 

m4 <- lm(formula = informal ~ trat20 + intervencao + (trat20 * intervencao)+ pib_df,
         data = data) 
 
stargazer(m1,m2,m3,m4, type = "text")
 
 m5 <- plm(formula = ocupado ~ trat20 + intervencao + (trat20 * intervencao),
                 model = "within",
                 index = c("setor"),
                 data = data) 
 
 m6 <- plm(formula = ocupado ~ trat20 + intervencao + (trat20 * intervencao),
           model = "within",
           index = c("setor"),
           data = data) 
 
 m7 <- plm(formula = ocupado ~ trat20 + intervencao_14_15 + intervencao_15_17 + intervencao_17_19 + 
             (trat20 * intervencao_14_15) + (trat20 * intervencao_15_17) + (trat20 * intervencao_17_19) + pib_df,
           model = "within",
           index = c("setor"),
           data = data) 
 
 stargazer(m5,m6,m7,type = "text") 
 
 
 m8 <- plm(formula = ocupado ~ trat20 + intervencao + (trat20 * intervencao) + pib_df + fem + idade + pessoas + mora_mesma_ra + escol_sup_com,
           model = "within",
           index = c("setor"),
           data = data) 
 
 stargazer(m8,type = "text") 
 
 