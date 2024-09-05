# Pacotes
library(tidyverse)
library(plm)
library(stargazer)

base <- readRDS("analysis/dados/base.RDS")

data <- base |> 
  filter(!is.na(ocupado),!is.na(escol_sup_com)) |> 
  select(ocupado,informal,ln_rend_bruto,ln_horas_trab,
         setor = CODSETOR2000,
         intervencao, trat20 = grupo_20,trat30 = grupo_30,
         ano,mes,ra = NM_SUBDIST, mora_mesma_ra,escol_sup_com,
         idade, escol_sup_com, fem, pessoas)

modelo <- plm(formula = ocupado ~ intervencao + trat20 + (trat20 * intervencao) + escol_sup_com + idade + fem,
    model = "within",
    index = c("setor","ano","mes"),
    data = data) 

stargazer(modelo,type = "text")



base |>  group_by(ano,grupo_20) |> summarise(n = n()) |> view()



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
           index = c("setor","ano"),
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
 
 