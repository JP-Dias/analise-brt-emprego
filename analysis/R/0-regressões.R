# Pacotes ----

library(tidyverse)
library(plm)
library(stargazer)
library(lubridate)
library(sidrar)
library(fixest)
library(zoo)
library(stargazer)
library(modelsummary)
library(haven)

# Leitura da base_gama de dados ----

dados <- readRDS("analysis/dados/base_ra.RDS")

dados2 <- dados |>
  filter(
    reg %in% c("Gama", "Ceilândia"),
    !(ano %in% c(2009,2018:2019))) |> 
  mutate(BRT_Effect = ifelse(aamm > "201406" & reg == "Gama", 1, 0),
         Treat = ifelse(reg == "Gama", 1, 0))

dados3 <- dados |>
  filter(
    reg %in% c("Santa Maria", "Recanto Das Emas"),
    !(ano %in% c(2009,2018:2019))) |> 
  mutate(BRT_Effect = ifelse(aamm > "201406" & reg == "Santa Maria", 1, 0),
         Treat = ifelse(reg == "Santa Maria", 1, 0))


# Main Results - Santa Maria ----

attach(dados3)

reg1 <- feols(log(rend_bruto) ~ BRT_Effect| reg + aamm, ~reg,weights = ~peso,data=dados3)


reg2 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2  |  reg + aamm + fem + cor + posicao_fam+ escol, ~reg,weights = ~peso, data=dados3)


reg3 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2  |  reg + aamm + fem + cor + escol + posicao_fam + setor_atv, ~reg,weights = ~peso, data=dados3)


modelsummary(list(reg1,reg2,reg3),
             coef_map = c("BRT_Effect" = "Efeito BRT","Treat"= "Tratado"),
             #output = "markdown",
             stars = T)

# Main Results - Gama ----

attach(dados2)

reg7 <- feols(log(rend_bruto) ~ BRT_Effect | reg + aamm, ~reg,weights = ~peso, data=dados2)


reg8 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg + aamm + fem + cor + posicao_fam+ escol, ~reg,weights = ~peso, data=dados2)


reg9 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg + aamm + fem + cor + escol + setor_atv + posicao_fam, ~reg,weights = ~peso, data=dados2)



modelsummary(list(reg7, reg8, reg9, reg1, reg2, reg3),
             #output = "latex_tabular", 
             coef_map = c("BRT_Effect" = "Efeito BRT","Treat"= "Tratado"),
             stars = T)

# Mechanisms - Santa Maria ----

reg13 <- feols(ocupado ~ BRT_Effect + idade + idade2 | reg + aamm + fem + cor + escol + posicao_fam, ~reg,weights = ~peso, data=dados3)
summary(reg13)

reg14 <- feols(informal ~ BRT_Effect + idade + idade2 | reg + aamm + fem  + cor + escol + setor_atv + posicao_fam, ~reg,weights = ~peso, data=dados3)
summary(reg14)

reg15 <- feols(trab_plano ~ BRT_Effect + idade + idade2 | reg + aamm + fem + cor + escol + setor_atv + posicao_fam,  ~reg,weights = ~peso, data=dados3)
summary(reg15)

reg16 <- feols(log(horas_trab) ~ BRT_Effect + idade + idade2 | reg + aamm + fem + cor + escol + setor_atv + posicao_fam,  ~reg,weights = ~peso, data=dados3)
summary(reg16)

modelsummary(list("Emprego"=reg13,
                  "Informal"=reg14,
                  "Emprego CBD"=reg15,
                  "(log) Horas trabalhadas"=reg16),
             coef_map = c("BRT_Effect" = "Efeito BRT"),
             #output = "latex_tabular",
             stars = T)

# Robustness - Santa Maria ----

## Event-Study Plot ----

reg13 <- feols(log(rend_bruto) ~ i(ano, Treat, 2013)  | reg + aamm 
               , ~ reg,weights = ~peso,
               data=dados3)
iplot(reg13)

reg14 <- feols(log(rend_bruto) ~ i(ano, Treat, 2013) + idade + idade2 | reg + aamm + escol + fem + setor_atv + cor + posicao_fam,
               cluster= c("reg"),weights = ~peso,
               data=dados3)
iplot(reg14,
      col = "darkblue", 
      sub = "Efeitos fixos reg e ano mes",
      main = "Teste")


modelo1 <- feols(log(rend_bruto) ~ i(ano, Treat, 2013) | reg + aamm, 
                 cluster = "reg",
                 weights = ~peso,
                 data = dados3)

modelo2 <- feols(log(rend_bruto) ~ i(ano, Treat, 2013) + idade + idade2 | reg + aamm + escol + fem + setor_atv + cor + posicao_fam,
                 cluster = "reg",
                 weights = ~peso,
                 data = dados3)

modelo3 <- feols(log(rend_bruto) ~ i(ano, Treat, 2013) | reg + aamm, 
                 cluster = "conglom",
                 weights = ~peso,
                 data = dados3)

modelo4 <- feols(log(rend_bruto) ~ i(ano, Treat, 2013) + idade + idade2 | reg + aamm + escol + fem + setor_atv + cor + posicao_fam,
                 cluster = "conglom",
                 weights = ~peso,
                 data = dados3)

par(mar = c(3, 4, 1, 2))

iplot(list(modelo1, modelo2), 
      col = c("darkblue", "darkred"), 
      pt.size = 1.5,
      ylab = "Efeito Log-Rendimento Bruto",
      xlab = "",
      sub = "",
      main = "")

legend("topleft",  
       legend = c("Modelo Simples", "Modelo com Controles"),
       col = c("darkblue", "darkred"), 
       pch = c(20, 17), 
       pt.cex = 1.5, 
       title = "Modelo")  

iplot(list(modelo3, modelo4), 
      col = c("darkgreen", "darkorange"), 
      pt.size = 1.5,
      ylab = "Efeito Log-Rendimento Bruto",
      xlab = "",
      sub = "",
      main = "")

legend("topleft",
       legend = c("Modelo Simples", "Modelo com Controles"), 
       col = c("darkgreen", "darkorange"),
       pch = c(20, 17),
       pt.cex = 1.5,  
       title = "Modelo")

iplot(list(modelo1, modelo2,modelo3, modelo4), 
      col = c("darkblue", "darkred", "darkgreen", "darkorange"), 
      pt.size = 1.5,
      ylab = "Efeito Log-Rendimento Bruto",
      xlab = "",
      sub = "",
      main = "")


legend("topleft",
       legend = c("Modelo Simples (bairro)", "Modelo com Controles (bairro)", "Modelo Simples (conglomerado)", "Modelo com Controles (conglomerado)"),  
       col = c("darkblue", "darkred", "darkgreen", "darkorange"),  
       pch = c(20, 17, 15, 1),  
       pt.cex = 1.5,  
       title = "Cluster de Erro Padrão")  
# 9.8 x 6.0

## Different SEs ----

reg15 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg + aamm + escol + fem + setor_atv + cor + posicao_fam,weights = ~peso, ~reg, data=dados3)
summary(reg15)

reg16 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg + aamm + escol + fem + setor_atv + cor + posicao_fam,weights = ~peso, vcov = "hetero", data=dados3)
summary(reg16)

reg17 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg + aamm + escol + fem + setor_atv + cor + posicao_fam,weights = ~peso, vcov = "iid", data=dados3)
summary(reg17)

reg18 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg + aamm + escol + fem + setor_atv + cor + posicao_fam,weights = ~peso,cluster = c("conglom","aamm"), data=dados3)
summary(reg18)

modelsummary(list(reg15,reg16,reg17,reg18),coef_map = c("BRT_Effect" = "Efeito BRT"),
             #output = "latex_tabular",
             stars = T)

## Heterogeneity ----
# Sexo
reg19 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg + aamm + escol + setor_atv + cor + posicao_fam ,weights = ~peso, ~reg, 
               data=dados3 |> filter(fem == 0))
summary(reg19)

reg20 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg + aamm + escol + setor_atv + cor + posicao_fam,weights = ~peso, ~reg, 
               data=dados3 |> filter(fem == 1))
summary(reg20)

# Raça
reg21 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg + aamm + escol + fem + setor_atv + posicao_fam,weights = ~peso, ~reg, 
               data=dados3 |> filter(negro == 1))
summary(reg21)

reg22 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg + aamm + escol + fem + setor_atv + posicao_fam,weights = ~peso, ~reg, 
               data=dados3 |> filter(negro == 0))
summary(reg22)

modelsummary(list("Homem" = reg19,
                  "Mulher" = reg20,
                  "Negro" = reg21,
                  "Não-negro" = reg22),
             coef_map = c("BRT_Effect" = "Efeito BRT"),
             #output = "latex_tabular",
             stars = T)

# Escolaridade

# Baixa Escolaridade
reg_baixa <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg + aamm + fem + setor_atv + cor + posicao_fam,
                   weights = ~peso, cluster = ~conglom,
                   data = dados3 |> filter(escol %in% c("analf", "fund_inc")))

# Ensino Fundamental Completo
reg_fund <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg + aamm + fem + setor_atv + cor + posicao_fam,
                  weights = ~peso, cluster = ~conglom,
                  data = dados3 |> filter(escol == "fund_com"))

# Ensino Médio
reg_medio <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg + aamm + fem + setor_atv + cor + posicao_fam,
                   weights = ~peso, cluster = ~conglom,
                   data = dados3 |> filter(escol %in% c("med_inc", "med_com")))

# Ensino Superior
reg_sup <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg + aamm + fem + setor_atv + cor + posicao_fam,
                 weights = ~peso, cluster = ~conglom,
                 data = dados3 |> filter(escol %in% c("sup_inc", "sup_com")))

# Sumário dos modelos
modelsummary(list("Baixa Escolaridade" = reg_baixa,
                  "Fundamental Completo" = reg_fund,
                  "Ensino Médio" = reg_medio,
                  "Ensino Superior" = reg_sup),
             coef_map = c("BRT_Effect" = "Efeito BRT"),
             #output = "latex_tabular",
             stars = T)

# Idade

reg33<- feols(log(rend_bruto) ~ BRT_Effect  | reg + aamm  + fem + setor_atv + cor + posicao_fam + escol,weights = ~peso, ~conglom, 
              data=dados3 |> filter(idade %in% 18:29))

reg34<- feols(log(rend_bruto) ~ BRT_Effect  | reg + aamm  + fem + setor_atv + cor + posicao_fam + escol,weights = ~peso, ~conglom, 
              data=dados3 |> filter(idade %in% 30:39))

reg35<- feols(log(rend_bruto) ~ BRT_Effect  | reg + aamm  + fem + setor_atv + cor + posicao_fam + escol,weights = ~peso, ~conglom, 
              data=dados3 |> filter(idade %in% 40:49))

reg36<- feols(log(rend_bruto) ~ BRT_Effect  | reg + aamm  + fem + setor_atv + cor + posicao_fam + escol,weights = ~peso, ~conglom, 
              data=dados3 |> filter(idade %in% 50:59))

reg37<- feols(log(rend_bruto) ~ BRT_Effect  | reg + aamm  + fem + setor_atv + cor + posicao_fam + escol,weights = ~peso, ~conglom, 
              data=dados3 |> filter(idade >= 60))


modelsummary(list("18-29"=reg33,
                  "30-39"=reg34,
                  "40-49"=reg35,
                  "50-59"=reg36,
                  "60+"=reg37),
             coef_map = c("BRT_Effect" = "Efeito BRT"),
             output = "latex_tabular",
             stars = T)

## Income ----

dados3 <- dados3 |> 
  group_by(aamm) |> 
  mutate(
    cent1 = quantile(rend_bruto,.99,na.rm = T),
    cent5 = quantile(rend_bruto,.95,na.rm = T),
    cent10 = quantile(rend_bruto,.9,na.rm = T)
    )


feols(rend_bruto ~ BRT_Effect + idade + idade2 | reg + aamm + fem + cor + escol + setor_atv + posicao_fam,  ~reg,weights = ~peso, data=dados3)


reg43 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg + aamm + fem + cor + escol + setor_atv + posicao_fam,  ~reg,weights = ~peso, data=dados3)


reg44 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg + aamm + fem + cor + escol + setor_atv + posicao_fam,  ~reg,weights = ~peso, data=dados3 |> filter(rend_bruto<cent1))


reg45 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg + aamm + fem + cor + escol + setor_atv + posicao_fam,  ~reg,weights = ~peso, data=dados3 |> filter(rend_bruto<cent5))


reg46 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg + aamm + fem + cor + escol + setor_atv + posicao_fam,  ~reg,weights = ~peso, data=dados3 |> filter(rend_bruto<cent10))

modelsummary(list("Completo" = reg43,
                  "Sem 1% mais ricos" = reg44,
                  "Sem 5% mais ricos" = reg45,
                  "Sem 10% mais ricos" = reg46),
coef_map = c("BRT_Effect" = "Efeito BRT"),
output = "default", #"latex_tabular",
stars = T)

reg47 <- feols(rend_bruto ~ BRT_Effect + idade + idade2 | reg + aamm + fem + cor + escol + setor_atv + posicao_fam,  ~reg,weights = ~peso, data=dados3)

reg48 <- feols(rend_bruto ~ BRT_Effect + idade + idade2 | reg + aamm + fem + cor + escol + setor_atv + posicao_fam,  ~reg,weights = ~peso, data=dados3 |> filter(rend_bruto<cent1))

reg49 <- feols(rend_bruto ~ BRT_Effect + idade + idade2 | reg + aamm + fem + cor + escol + setor_atv + posicao_fam,  ~reg,weights = ~peso, data=dados3 |> filter(rend_bruto<cent5))

reg50 <- feols(rend_bruto ~ BRT_Effect + idade + idade2 | reg + aamm + fem + cor + escol + setor_atv + posicao_fam,  ~reg,weights = ~peso, data=dados3 |> filter(rend_bruto<cent10))

modelsummary(list(reg47,reg48,reg49,reg50),
             coef_map = c("BRT_Effect" = "Efeito BRT"),
             output = "default", #"latex_tabular",
             stars = T)

## Diferent control regions ----

dados4 <- dados |>
  filter(
    reg %in% c("Santa Maria", "Riacho Fundo"),
    !(ano %in% c(2009,2018:2019))) |> 
  mutate(BRT_Effect = ifelse(aamm > "201406" & reg == "Santa Maria", 1, 0),
         Treat = ifelse(reg == "Santa Maria", 1, 0))
  
dados5 <- dados |>
  filter(
    reg %in% c("Santa Maria", "Paranoá"),
    !(ano %in% c(2009,2018:2019))) |> 
  mutate(BRT_Effect = ifelse(aamm > "201406" & reg == "Santa Maria", 1, 0),
         Treat = ifelse(reg == "Santa Maria", 1, 0))

dados6 <- dados |>
  filter(
    reg %in% c("Santa Maria", "Núcleo Bandeirante"),
    !(ano %in% c(2009,2018:2019))) |> 
  mutate(BRT_Effect = ifelse(aamm > "201406" & reg == "Santa Maria", 1, 0),
         Treat = ifelse(reg == "Santa Maria", 1, 0))




reg51 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg + aamm + fem + cor + escol + setor_atv + posicao_fam,  ~reg,weights = ~peso, 
      data=dados3)

reg52 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg + aamm + fem + cor + escol + setor_atv + posicao_fam,  ~reg,weights = ~peso, 
      data=dados4)


reg53 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg + aamm + fem + cor + escol + setor_atv + posicao_fam,  ~reg,weights = ~peso, 
      data=dados5)

reg54 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg + aamm + fem + cor + escol + setor_atv + posicao_fam,  ~reg,weights = ~peso, 
      data=dados6)


modelsummary(list("Nearest" = reg51,
                  "Second Nearest" = reg52,
                  "Third Nearest" = reg53),
             coef_map = c("BRT_Effect" = "Efeito BRT"),
             output = "default", #"latex_tabular",
             stars = T)

regioes <- unique(dados$reg)[-c(1:2,13:14,21)]

lista <- list() 

for (i in regioes) {
  # Filtra os dados para incluir apenas Santa Maria e a região iterada
  temp <- dados |>
    filter(
      reg %in% c("Santa Maria", i),  # Agora filtra corretamente Santa Maria e a região atual
      !(ano %in% c(2009, 2018:2019))) |> 
    mutate(
      BRT_Effect = ifelse(aamm > "201406" & reg == "Santa Maria", 1, 0),
      Treat = ifelse(reg == "Santa Maria", 1, 0)
    )
  
  # Estima o modelo e armazena o coeficiente do efeito BRT
  modelo <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | 
                    reg + aamm + fem + cor + escol + setor_atv + posicao_fam,  
                  ~reg, weights = ~peso, data=temp)
  
  # Armazena o coeficiente do efeito BRT na lista
  lista[[i]] <- data.frame(reg = i, coef = coefficients(modelo)["BRT_Effect"])
}

# Exibir a lista de coeficientes
teste <- bind_rows(lista)



for (i in regioes) {
  # Filtra os dados para incluir apenas Santa Maria e a região iterada
  temp <- dados |>
    filter(
      reg %in% c("Santa Maria", i),  # Agora filtra corretamente Santa Maria e a região atual
      !(ano %in% c(2009, 2018:2019))) |> 
    mutate(
      BRT_Effect = ifelse(aamm > "201406" & reg == "Santa Maria", 1, 0),
      Treat = ifelse(reg == "Santa Maria", 1, 0)
    )
  
  # Estima o modelo e armazena o coeficiente do efeito BRT
  modelo <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | 
                    reg + aamm + fem + cor + escol + setor_atv + posicao_fam,  
                  ~reg, weights = ~peso, data=temp)

  print(i)
  print(modelo)

}

source("config.R")
library(mapview)
library(paletteer)

malha_2010 <- st_read(build("Shapes/2010/53SEE250GC_SIR.shp"), options = "ENCODING=WINDOWS-1252")

malha_2010 <- st_set_crs(malha_2010, 4674)  

# Cria malha de Subdistritos para mapas
malha_subdist<- malha_2010  |> 
  mutate(reg = ifelse(NM_SUBDIST == "BRASÍLIA","Plano Piloto",str_to_title(NM_SUBDIST))) |>
  st_make_valid() |> 
  group_by(reg) |> 
  summarise(geometry = st_union(geometry)) |> 
  left_join(teste)

mapview(malha_subdist, zcol = "coef",col.regions = paletteer_c("ggthemes::Classic Red", 20))
mapview(malha_subdist, zcol = "coef",col.regions = paletteer_c("grDevices::Oslo", 20))


