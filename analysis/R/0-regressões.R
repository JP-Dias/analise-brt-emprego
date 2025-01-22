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

# Leitura da base_gama de dados
dados <- readRDS("analysis/dados/base_ra.RDS") |> filter(reg %in% c("Gama","Santa Maria","Recanto Das Emas","Brazlândia"))
# 
# dados <- dados |> filter(reg%in%"Santa Maria")
# 
# teste <- dados |> filter(!is.na(rend_bruto)) |> as_survey(weights = peso)
# 
# gini <- function(x){data.frame(ano = x, gini = convey::svygini(~rend_bruto,convey_prep(teste |> filter(ano == x)))[1])}
# 
# gini(2010)
# 
# anos <- unique(dados$ano)
# 
# lapply(anos, gini) |> bind_rows() |> plot()

dados2 <- subset(dados, reg %in% c("Gama", "Brazlândia"))

dados3 <- subset(dados, reg %in% c("Santa Maria", "Recanto Das Emas"))

dados3 <- subset(dados3, !ano %in% c(2009, 2018, 2019))
dados2 <- subset(dados2, !ano %in% c(2009, 2018, 2019))


### Create a Variable of Interest ###
dados1 <- dados

dados1$BRT_Effect <- ifelse(dados1$aamm > "201406" & dados$reg %in% c("Santa Maria","Gama"), 1, 0)
dados1$Treat <- ifelse(dados1$reg %in% c("Santa Maria","Gama"), 1, 0)

dados3$BRT_Effect <- ifelse(dados3$aamm > "201406" & dados3$reg == "Santa Maria", 1, 0)
dados3$Treat <- ifelse(dados3$reg == "Santa Maria", 1, 0)

dados2$BRT_Effect <- ifelse(dados2$aamm > "201406" & dados2$reg == "Gama", 1, 0)
dados2$Treat <- ifelse(dados2$reg == "Gama", 1, 0)


#### Main Results - Santa Maria ####

reg0 <- feols(log(rend_bruto) ~ BRT_Effect | reg + aamm, ~reg,weights = ~peso,data=dados1)
reg01 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg + aamm + fem + cor + escol + posicao_fam + setor_atv, ~conglom,weights = ~peso,data=dados1)

modelsummary(list(reg7, reg8, reg9, reg1, reg2, reg3),
             output = "latex_tabular",
             stars = T)

attach(dados3)

reg1 <- feols(log(rend_bruto) ~ BRT_Effect| reg + aamm, ~reg,weights = ~peso,data=dados3)
summary(reg1)

reg2 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2  |  reg + aamm + fem + cor + posicao_fam, ~reg,weights = ~peso, data=dados3)
summary(reg2)

reg3 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2  |  reg + aamm + fem + cor + escol + posicao_fam + setor_atv, ~reg,weights = ~peso, data=dados3)
summary(reg3)

reg4 <- feols(log(rend_bruto) ~ BRT_Effect | reg + aamm, ~conglom,weights = ~peso,data=dados3)
summary(reg4)

reg5 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg +  aamm + fem + cor + posicao_fam, ~conglom,weights = ~peso, data=dados3)
summary(reg5)

reg6 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2| reg + aamm + fem + cor + escol + posicao_fam + setor_atv, ~conglom,weights = ~peso, data=dados3)
summary(reg6)


modelsummary(list(reg1,reg2,reg3,reg4,reg5,reg6),
             coef_map = c("BRT_Effect" = "Efeito BRT","Treat"= "Tratado"),
             output = "markdown",
             stars = T)


### Main Results - Gama ####

attach(dados2)

feols(ocupado ~ BRT_Effect | reg + aamm, ~reg,weights = ~peso,data=dados2)
feols(informal ~ BRT_Effect | reg + aamm, ~reg,weights = ~peso,data=dados2)
feols(horas_trab ~ BRT_Effect + idade + idade2 | reg + aamm + fem + cor + escol + setor_atv + posicao_fam, ~conglom,weights = ~peso, data=dados2)

reg7 <- feols(log(rend_bruto) ~ BRT_Effect | reg + aamm, ~reg,weights = ~peso, data=dados2)
summary(reg7)

reg8 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg + aamm + fem + cor + posicao_fam, ~reg,weights = ~peso, data=dados2)
summary(reg6)

reg9 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg + aamm + fem + cor + escol + setor_atv + posicao_fam, ~reg,weights = ~peso, data=dados2)
summary(reg8)

reg10 <- feols(log(rend_bruto) ~ BRT_Effect | reg + aamm, ~conglom,weights = ~peso, data=dados2)
summary(reg5)

reg11 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg + aamm + fem + cor + posicao_fam, ~conglom,weights = ~peso, data=dados2)
summary(reg6)

reg12 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg + aamm + fem + cor + escol + setor_atv + posicao_fam, ~conglom,weights = ~peso, data=dados2)
summary(reg8)

modelsummary(list(reg7,reg8,reg9,reg10,reg11,reg12),coef_map = c("BRT_Effect" = "Efeito BRT"),
             #output = "latex_tabular",
             stars = T)

  ##### Mechanisms - Santa Maria ######

reg13 <- feols(ocupado ~ BRT_Effect + idade + idade2 | reg + aamm + fem + cor + escol + posicao_fam, ~reg,weights = ~peso, data=dados3)
summary(reg13)

reg14 <- feols(informal ~ BRT_Effect + idade + idade2 | reg + aamm + fem + cor + escol + setor_atv + posicao_fam, ~reg,weights = ~peso, data=dados3)
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
             output = "latex_tabular",
             stars = T)

#### Robustness - Santa Maria ####

### Event-Study Plot  ###

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


# 9.8 x 6.0

iplot(
  reg14,
  col = "darkblue",             # Cor das barras
  sub = "Efeitos fixos de região e ano-mês", # Subtítulo refinado
  main = "",                    # Título mais descritivo (deixe vazio se desnecessário)
  xlab = "Ano de Referência",   # Rótulo do eixo x
  ylab = "Efeito Log-Rendimento Bruto", # Rótulo do eixo y
  zero.line = TRUE,             # Linha de referência em zero
  col.zero = "gray50",          # Cor da linha zero
  pt.size = 1.5,                # Tamanho dos pontos
  pt.col = "grey10",            # Cor dos pontos
  ci.lwd = 2,                   # Espessura das linhas de intervalo de confiança
  ci.col = "grey50",            # Cor das linhas de intervalo de confiança
  ci.alpha = 0.3,               # Transparência das áreas de intervalo de confiança
  grid = TRUE,                  # Adicionar linhas de grade
  grid.col = "gray90",          # Cor da grade
  lwd = 2,                      # Espessura das barras
  theme = list(                 # Personalização adicional de texto
    text.size = 14,             # Tamanho do texto
    text.family = "Latin Modern Roman" # Fonte refinada para publicação
  )
)



iplot(
  reg14,
  col = "darkblue",                  # Cor das barras
  ci.col = "gray60",                 # Cor das linhas de intervalo de confiança
  pt.col = "black",                  # Cor dos pontos
  pt.pch = 16,                       # Tipo de ponto: círculo preenchido
  pt.cex = 1.5,                      # Tamanho dos pontos
  ci.lwd = 2,                        # Espessura das linhas de intervalo de confiança
  ci.lty = 1,                        # Tipo de linha para os intervalos de confiança
  zero = TRUE,                       # Linha de referência no zero
  zero.par = list(col = "gray50", lwd = 1), # Personalização da linha de referência
  xlab = "Ano de Referência",        # Rótulo do eixo X
  ylab = "Efeito Log-Rendimento Bruto", # Rótulo do eixo Y
  main = "",                         # Sem título no gráfico principal
  sub = "Efeitos fixos de região e ano-mês", # Subtítulo refinado
  grid = TRUE,                       # Adiciona grades
  grid.par = list(lty = 3, col = "gray85"),  # Personalização das grades
  ci.fill = TRUE,                    # Preenchimento dos intervalos de confiança
  ci.fill.par = list(col = "lightblue", alpha = 0.1), # Cor e transparência do preenchimento
  horiz = FALSE                      # Mantém o gráfico na orientação vertical
)



### Different SEs ###

reg15 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg + aamm + escol + fem + setor_atv + cor + posicao_fam,weights = ~peso, ~conglom, data=dados3)
summary(reg15)

reg16 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg + aamm + escol + fem + setor_atv + cor + posicao_fam,weights = ~peso, vcov = "hetero", data=dados3)
summary(reg16)

reg17 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg + aamm + escol + fem + setor_atv + cor + posicao_fam,weights = ~peso, vcov = "iid", data=dados3)
summary(reg17)

reg18 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg + aamm + escol + fem + setor_atv + cor + posicao_fam,weights = ~peso,cluster = c("conglom","aamm"), data=dados3)
summary(reg18)



modelsummary(list(reg15,reg16,reg17,reg18),coef_map = c("BRT_Effect" = "Efeito BRT"),
             output = "latex_tabular",
             stars = T)

### Heterogeneity ###

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

# Posição Familha
reg23 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg + aamm + escol + fem + setor_atv + cor ,weights = ~peso, ~reg, 
               data=dados3 |> filter(posicao_fam == "chefe"))
summary(reg23)

reg24 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg + aamm + escol + fem + setor_atv + cor ,weights = ~peso, ~reg, 
               data=dados3 |> filter(posicao_fam == "conjuge"))
summary(reg24)

reg25 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg + aamm + escol + fem + setor_atv + cor ,weights = ~peso, ~reg, 
               data=dados3 |> filter(posicao_fam == "filho"))
summary(reg25)


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
             output = "latex_tabular",
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

# Endogeneidade #######

reg38 <- feols(negro ~ BRT_Effect + idade + idade2 | reg + aamm + fem  + escol + posicao_fam  , ~reg,weights = ~peso, data=dados3)


reg39 <- feols(en_sup ~ BRT_Effect + idade + idade2 | reg + aamm + fem + cor + posicao_fam, ~reg,weights = ~peso, data=dados3 |> filter(idade>35))


reg40 <- feols(trab_plano ~ BRT_Effect + idade + idade2 | reg + aamm + fem + cor + escol + posicao_fam,  ~reg,weights = ~peso, data=dados3)


#reg41 <- feols(log(mora_mesma_ra) ~ BRT_Effect + idade + idade2 | reg + aamm + fem + cor + escol + setor_atv + posicao_fam,  ~reg,weights = ~peso, data=dados3)


modelsummary(list("Negro"=reg38,
                  "Ensino Superior"=reg39,
                  "Trabalha no CBD"=reg40#,
                  #"Mora na RA"=reg41
                  ),
             coef_map = c("BRT_Effect" = "Efeito BRT"),
             output = "default", #"latex_tabular",
             stars = T)

reg42 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2 | reg + aamm + fem + cor + escol + setor_atv + posicao_fam,  ~reg,weights = ~peso, data=dados3 |> filter(informal ==0))

summary(reg42)
