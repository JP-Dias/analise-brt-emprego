# Pacotes ----

library(tidyverse)
library(plm)
library(stargazer)
library(lubridate)
library(fixest)
library(zoo)
library(stargazer)
library(modelsummary)
library(haven)

dados <- readRDS("analysis/dados/base_ra.RDS")

dados_ra <- dados |>
  filter(
    !(reg %in% c("Plano Piloto", "Lago Norte","Lago Sul","Brasília","Gama")),
    !(ano %in% c(2009,2018:2019))) |> 
  mutate(BRT_Effect = ifelse(aamm > "201406" & reg == "Santa Maria", 1, 0),
         Treat = ifelse(reg == "Santa Maria", 1, 0),
         idoso = ifelse(idade >= 60,1,0),
         analf = ifelse(escol == "analf",1,0)) |> 
  group_by(aamm,reg,domic) |> 
  mutate(renda_dom = sum(rend_bruto,na.rm = T)) |> 
  ungroup()

dados_ra |> 
  
  
  group_by(aamm,reg) |> 
  
feols(c(mora_mesma_ra,en_sup,pessoas,fem,negro,idoso,analf,renda_dom) ~ BRT_Effect | reg + aamm, ~reg, weights = ~peso, data = dados_ra)

modelsummary(list(feols(c(mora_mesma_ra,en_sup,pessoas,fem,negro,idoso,analf,renda_dom) ~ BRT_Effect | setor_atv + reg + aamm, ~reg, weights = ~peso, data = dados_ra)),
             output = "markdown", 
             coef_map = c("BRT_Effect" = "Efeito BRT","Treat"= "Tratado"),
             stars = T)


library(Synth)
library(haven)
library(zoo)
library(Hmisc)

# Leitura da base de dados
base <- readRDS("analysis/dados/base_ra.RDS") 

# Criação da Variável de Efeito ----


dados <- base |> 
  filter(!is.na(ocupado), idade %in% c(18:64)) |>  # Filtra ocupados, entre 18 e 64 anos e que moram na mesma RA nos últimos 12 meses 
  mutate(painel = case_when(mes %in% c(1,4,7,10)~"A",
                            mes %in% c(2,5,8,11)~"B",
                            TRUE~"C")) # Cria uma variável que sinaliza o Painel (A PED é feita em 3 paineis rotativos)

dados <- fastDummies::dummy_cols(dados,"escol")
dados <- fastDummies::dummy_cols(dados,"cor")

dados1 <- dados |> 
  mutate(idoso = ifelse(idade>=60,1,0),
         reg = ifelse(reg == "Brasília","Plano Piloto",reg)) |> 
  group_by(reg,ano) |> 
  summarise(
    mesma_ra = wtd.mean(mora_mesma_ra,peso,na.rm = T),
    analf = wtd.mean(escol_analf,peso,na.rm = T),
    sup = wtd.mean(escol_sup_com,peso,na.rm = T),
    idoso = wtd.mean(idoso,peso,na.rm = T),
    informal = wtd.mean(informal, peso,na.rm = T),
    pessoas = wtd.mean(pessoas, peso,na.rm = T),
    fem = wtd.mean(fem,peso),
    branca = wtd.mean(cor_branca,peso),
    preta = wtd.mean(cor_preta,peso),
    parda = wtd.mean(cor_parda,peso),
    rend_bruto = wtd.mean(rend_bruto,peso, na.rm = T),
    ocupado = wtd.mean(ocupado,peso,na.rm = T),
    horas = wtd.mean(horas_trab,peso,na.rm = T)
  ) |> 
  ungroup() |> 
  filter(
    !(reg %in% c("Gama")),
    !(ano %in% c(2009,2018:2019))) |> 
  mutate(BRT_Effect = ifelse(ano > 2013 & reg == "Santa Maria", 1, 0)) |> 
  mutate(reg_id = as.numeric(as.factor(reg))) |> 
  as.data.frame() |> na.omit()

dados1

feols(c(mesma_ra,sup,pessoas,fem,preta,parda,branca,idoso,analf) ~ BRT_Effect | reg + ano, ~reg, data = dados1)

modelsummary(list(feols(c(mesma_ra,sup,fem,preta,parda,branca,idoso,analf) ~ BRT_Effect | reg + ano, ~reg, data = dados1)),
             output = c("default"), 
             coef_map = c("BRT_Effect" = "Efeito BRT","Treat"= "Tratado"),
             star = T)

