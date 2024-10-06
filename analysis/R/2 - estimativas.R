# Pacotes ----
library(tidyverse)
library(plm)
library(fixest)
library(stargazer)

dados <- readRDS("analysis/dados/base.RDS")

#### Montar Variável de Interesse ###

dados <- dados[!is.na(dados$ocupado), ]

attach(dados)

dados$Effect <- grupo_20 * intervencao

# Estimativas Iniciais ----

reg1 <- feols(ocupado ~ Effect | setor + ano , ~setor, data=dados)
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


#### Event-Study TWFE ####

reg2 <- feols(trab_plano ~ i(ano, grupo_20, 2013)| setor + ano, 
              data=dados)
summary(reg2, se="twoway")

iplot(reg2, se="twoway")


reg2 <- feols(rend_bruto ~ i(ano, grupo_20, 2013)| CODSETOR2000 + ano, 
              data=dados)
summary(reg2, se="twoway")

iplot(reg2, se="twoway",col = "darkblue", sub = "Efeitos fixos setor e ano")

# summary(reg1, se="twoway")
# summary(reg1)