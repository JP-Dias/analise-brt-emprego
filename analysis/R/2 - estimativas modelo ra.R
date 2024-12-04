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
base <- readRDS("analysis/dados/base_ra.RDS") |> filter(reg %in% c("Gama","Santa Maria","Recanto Das Emas","Sobradinho"))

# Criação da Variável de Efeito ----

## Gama ----
dados_gama <- base |> filter(reg %in% c("Gama","Recanto Das Emas")) |> 
  mutate(grupo = ifelse(reg == "Gama",1,0),
         Effect = grupo * intervencao) |> # Montar Variável de Interesse
  filter(!is.na(ocupado)) 

# Santa Maria ----
dados_sm <- base |> filter(reg %in% c("Santa Maria","Sobradinho")) |>
  mutate(grupo = ifelse(reg == "Santa Maria",1,0),
         Effect = grupo * intervencao) |> # Montar Variável de Interesse
  filter(!is.na(ocupado))

# Estimativas ----
## Gama ----

### Estimativas Iniciais ----
reg1 <- feols(ocupado ~ Effect | reg + ano , ~reg, data=dados_gama)
reg2 <- feols(informal ~ Effect | reg + ano , ~reg, data=dados_gama)
reg3 <- feols(ln_rend_bruto ~ Effect | reg + ano , ~reg, data=dados_gama)
reg4 <- feols(ln_horas_trab ~ Effect | reg + ano , ~reg, data=dados_gama)

modelsummary(list(
  "Ocupado | Setor"=reg1,
  "Informal | Setor"=reg2,
  "Ln Renda | Setor"=reg3,
  "Ln Horas | Setor"=reg4),
  output ="default",
  stars = T)

### Estimativas com variáveis de controle ----

reg1_ctrl <- feols(ocupado ~ Effect + idade + idade2 + fem + negro + en_sup | reg + ano , ~reg, data=dados_gama)
reg2_ctrl <- feols(informal ~ Effect + idade + idade2 + fem + negro + en_sup | reg + ano , ~reg, data=dados_gama)
reg3_ctrl <- feols(ln_rend_bruto ~ Effect + idade + idade2 + fem + negro + en_sup | reg + ano , ~reg, data=dados_gama)
reg4_ctrl <- feols(ln_horas_trab ~ Effect + idade + idade2 + fem + negro + en_sup | reg + ano , ~reg, data=dados_gama)


# Tabela de resumo 
modelsummary(list(
  "Ocupado | Setor (Ctrl)" = reg1_ctrl,
  "Informal | Setor (Ctrl)" = reg2_ctrl,
  "Ln Renda | Setor (Ctrl)" = reg3_ctrl,
  "Ln Horas | Setor (Ctrl)" = reg4_ctrl), 
  output = "default", stars = TRUE)


## Santa Maria
### Estimativas Iniciais ----

reg1 <- feols(ocupado ~ Effect | reg + ano , ~reg, data=dados_sm)
reg2 <- feols(informal ~ Effect | reg + ano , ~reg, data=dados_sm)
reg3 <- feols(ln_rend_bruto ~ Effect | reg + ano , ~reg, data=dados_sm)
reg4 <- feols(ln_horas_trab ~ Effect | reg + ano , ~reg, data=dados_sm)

modelsummary(list(
  "Ocupado | Setor"=reg1,
  "Informal | Setor"=reg2,
  "Ln Renda | Setor"=reg3,
  "Ln Horas | Setor"=reg4),
  output ="default",
  stars = T)


### Estimativas com variáveis de controle ----

reg1_ctrl <- feols(ocupado ~ Effect + idade + idade2 + fem + negro + en_sup | reg + ano , ~reg, data=dados_sm)
reg2_ctrl <- feols(informal ~ Effect + idade + idade2 + fem + negro + en_sup | reg + ano , ~reg, data=dados_sm)
reg3_ctrl <- feols(ln_rend_bruto ~ Effect + idade + idade2 + fem + negro + en_sup | reg + ano , ~reg, data=dados_sm)
reg4_ctrl <- feols(ln_horas_trab ~ Effect + idade + idade2 + fem + negro + en_sup | reg + ano , ~reg, data=dados_sm)


# Tabela de resumo 
modelsummary(list(
  "Ocupado | Setor (Ctrl)" = reg1_ctrl,
  "Informal | Setor (Ctrl)" = reg2_ctrl,
  "Ln Renda | Setor (Ctrl)" = reg3_ctrl,
  "Ln Horas | Setor (Ctrl)" = reg4_ctrl), 
  output = "default", stars = TRUE)