# Pacotes ----
library(tidyverse)
library(plm)
library(fixest)
library(stargazer)
library(modelsummary)

dados <- readRDS("analysis/dados/base.RDS")

#### Montar Variável de Interesse ###

dados <- dados[!is.na(dados$ocupado), ]
#dados <- dados |> filter(NM_SUBDIST == "GAMA")

attach(dados)

dados$Effect <- grupo_20 * intervencao

dados |> 
  as_survey(peso) |> 
  group_by(NM_SUBDIST ,bairro,intervencao) |> 
  summarise(renda = survey_mean(rend_bruto,na.rm = T)) |> 
  ggplot(aes(x = bairro,y = renda,fill = factor(intervencao))) + 
  geom_col(position = "dodge") +
  facet_wrap(~NM_SUBDIST)


# Estimativas Iniciais ----

reg1 <- feols(log(rend_bruto) ~ Effect | intervencao + bairro + ano + cor + escol + posicao_fam, ~conglom, data=dados)

reg1

reg2 <- feols(informal ~ Effect | setor + ano, ~setor, data=dados)
reg3 <- feols(ln_rend_bruto ~ Effect | setor + ano, ~setor, data=dados)
reg4 <- feols(ln_horas_trab ~ Effect | setor + ano, ~setor, data=dados)

reg5 <- feols(ocupado ~ Effect | bairro + ano, ~bairro, data=dados)
reg6 <- feols(informal ~ Effect | bairro + ano, ~bairro, data=dados)
reg7 <- feols(ln_rend_bruto ~ Effect | bairro + ano, ~bairro, data=dados)
reg8 <- feols(ln_horas_trab ~ Effect | bairro + ano, ~bairro, data=dados)

# Tabela de resumo 
modelsummary(list(
  "Ocupado | Setor"=reg1,
  "Informal | Setor"=reg2,
  "Ln Renda | Setor"=reg3,
  "Ln Horas | Setor"=reg4,
  #
  "Ocupado | Bairro"=reg5,
  "Informal | Bairro"=reg6,
  "Ln Renda | Bairro"=reg7,
  "Ln Horas | Bairro"=reg8
  ),
  output ="default",
  stars = T)

# Estimativas com variáveis de controle ----

reg1_ctrl <- feols(ocupado ~ Effect + idade + idade2 + fem + negro + en_sup | setor + ano , ~setor, data=dados)
reg2_ctrl <- feols(informal ~ Effect + idade + idade2 + fem + negro + en_sup | setor + ano , ~setor, data=dados)
reg3_ctrl <- feols(ln_rend_bruto ~ Effect + idade + idade2 + fem + negro + en_sup | setor + ano , ~setor, data=dados)
reg4_ctrl <- feols(ln_horas_trab ~ Effect + idade + idade2 + fem + negro + en_sup | setor + ano , ~setor, data=dados)

reg5_ctrl <- feols(ocupado ~ Effect + idade + idade2 + fem + negro + en_sup | bairro + ano , ~bairro, data=dados)
reg6_ctrl <- feols(informal ~ Effect + idade + idade2 + fem + negro + en_sup | bairro + ano , ~bairro, data=dados)
reg7_ctrl <- feols(ln_rend_bruto ~ Effect + idade + idade2 + fem + negro + en_sup | bairro + ano , ~bairro, data=dados)
reg8_ctrl <- feols(ln_horas_trab ~ Effect + idade + idade2 + fem + negro + en_sup | bairro + ano , ~bairro, data=dados)

# Tabela de resumo 
modelsummary(list(
  "Ocupado | Setor (Ctrl)" = reg1_ctrl,
  "Informal | Setor (Ctrl)" = reg2_ctrl,
  "Ln Renda | Setor (Ctrl)" = reg3_ctrl,
  "Ln Horas | Setor (Ctrl)" = reg4_ctrl,
  "Ocupado | Bairro (Ctrl)" = reg5_ctrl,
  "Informal | Bairro (Ctrl)" = reg6_ctrl,
  "Ln Renda | Bairro (Ctrl)" = reg7_ctrl,
  "Ln Horas | Bairro (Ctrl)" = reg8_ctrl
), 
output = "default", stars = TRUE)


# Event-Study TWFE ----
## Ocupado ----
reg_ocupado_setor <- feols(ocupado ~ i(ano, grupo_20, 2013)| setor + ano, 
              data=dados)
summary(reg_ocupado_setor, se="twoway")

iplot(reg_ocupado_setor,col = "darkred", se="twoway", main="Efeito sobre Ocupado (EF Setor)")

reg_ocupado_bairro <- feols(ocupado ~ i(ano, grupo_20, 2013)| bairro + ano, 
              data=dados)
summary(reg_ocupado_bairro, se="twoway")

iplot(reg_ocupado_bairro,col = "darkblue", se="twoway",main = "Efeito sobre Ocupado (EF Bairro)")

## Informal ----
reg_informal_setor <- feols(informal ~ i(ano, grupo_20, 2013)| setor + ano, 
                            data=dados)
summary(reg_informal_setor, se="twoway")

iplot(reg_informal_setor,col = "darkred", se="twoway", main="Efeito sobre informal (EF Setor)")

reg_informal_bairro <- feols(informal ~ i(ano, grupo_20, 2013)| bairro + ano, 
                             data=dados)
summary(reg_informal_bairro, se="twoway")

iplot(reg_informal_bairro,col = "darkblue", se="twoway",main = "Efeito sobre informal (EF Bairro)")

## Rendimento ----
reg_ln_rend_bruto_setor <- feols(ln_rend_bruto ~ i(ano, grupo_20, 2013)| setor + ano, 
                           data=dados)
summary(reg_ln_rend_bruto_setor, se="twoway")

iplot(reg_ln_rend_bruto_setor,col = "darkred", se="twoway", main="Efeito sobre ln_rend_bruto (EF Setor)")

reg_ln_rend_bruto_bairro <- feols(ln_rend_bruto ~ i(ano, grupo_20, 2013)| bairro + ano, 
                            data=dados)
summary(reg_ln_rend_bruto_bairro, se="twoway")

iplot(reg_ln_rend_bruto_bairro,col = "darkblue", se="twoway",main = "Efeito sobre ln_rend_bruto (EF Bairro)")

## Horas Trabalhadas ----
reg_ln_horas_trab_setor <- feols(ln_horas_trab ~ i(ano, grupo_20, 2013)| setor + ano, 
                                 data=dados)
summary(reg_ln_horas_trab_setor, se="twoway")

iplot(reg_ln_horas_trab_setor,col = "darkred", se="twoway", main="Efeito sobre ln_horas_trab (EF Setor)")

reg_ln_horas_trab_bairro <- feols(ln_horas_trab ~ i(ano, grupo_20, 2013)| bairro + ano, 
                                  data=dados)
summary(reg_ln_horas_trab_bairro, se="twoway")

iplot(reg_ln_horas_trab_bairro,col = "darkblue", se="twoway",main = "Efeito sobre ln_horas_trab (EF Bairro)")










# reg2 <- feols(rend_bruto ~ i(ano, grupo_20, 2013)| CODSETOR2000 + ano, 
#               data=dados)
# summary(reg2, se="twoway")
# 
# iplot(reg2, se="twoway",col = "darkblue", sub = "Efeitos fixos setor e ano")

# summary(reg1, se="twoway")
# summary(reg1)