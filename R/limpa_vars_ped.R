#'*Prepara Microdados PED*

# Pacotes ----
library(haven)
library(tidyverse)
library(dplyr)
library(labelled)

# PED
ped <- readRDS("Rds/ped_empilhada.RDS")

# Variáveis

# Supondo que o dataset já esteja carregado no objeto `dados_ped`

# Seleção e renomeação das variáveis
dados <- ped |> 
  mutate(
    informal = ifelse(pos == 2 | (pos %in% 5:6 & pos == 2), 1, 0),
    trab_plano = ifelse(q270 == 5301, 1, 0),
    mora_mesma_ra = ifelse(c071 == 1, 1, 0),
    ocupado = case_when(
      sit == 4 ~ 1,
      sit %in% 1:3 ~ 0,
      TRUE ~ NA_integer_
    ),
    rend_bruto = ifelse(as.numeric(q421) %in% c(0, 1e7 + 1), NA_integer_, as.numeric(q421)),
    rend_liquido = ifelse(as.numeric(q422) %in% c(0, 1e7 + 1), NA_integer_, as.numeric(q422)),
    horas_trab = ifelse(as.numeric(q431) %in% c(1e3:1e3 + 1), NA_integer_, as.numeric(q431)),
    escol = case_when(
      inst == 2 ~ "analf",
      inst == 3 ~ "sem_esc",
      inst == 4 ~ "fund_inc",
      inst == 5 ~ "fund_com",
      inst == 6 ~ "med_inc",
      inst == 7 ~ "med_com",
      inst == 8 ~ "sup_inc",
      inst == 9 ~ "sup_com",
      TRUE ~ NA_character_
    ),
    setor_atv = case_when(
      setor == 200 ~ "indust",
      setor == 300 ~ "construc",
      setor == 400 ~ "comerc",
      setor == 500 ~ "servic",
      setor == 511 ~ "servic_dom",
      setor == 600 ~ "outros",
      TRUE ~ NA_character_
    ),
    cor = case_when(
      c050 == 1 ~ "branca",
      c050 == 2 ~ "preta",
      c050 == 3 ~ "parda",
      c050 == 4 ~ "amarela",
      c050 == 0 ~ "sd",
      TRUE ~ NA_character_
    ),
    posicao_fam = case_when(
      c040 == 1 ~ "chefe",
      c040 == 2 ~ "conjuge",
      c040 == 3 ~ "filho",
      TRUE ~ "outros"
    )
  ) |> 
  select(
    ano, mes, # Ano e Mês
    conglom = conglom, # Conglomerado
    domic = domic, # Domicílio
    ocupado, # Situação ocupacional
    rend_bruto, # Rendimento bruto
    rend_liquido, # Rendimento líquido
    informal, # Trabalhador Informal
    horas_trab, # Horas trabalhadas no trabalho principal
    trab_plano, # Dummy de trabalha na RA Plano Piloto
    setor_atv, # Setor de atividade CNAE
    escol, # Grau de instrução
    idade = c020, # Idade
    raca = c050, # Cor
    sexo = c010, # Sexo
    mora_mesma_ra, # Mora na Messma RA 12 meses atrás
    pessoas = a090, # Total de moradores no domicílio
    posicao_fam # Posição na família
  )

saveRDS(dados,"Rds/ped_vars.RDS")

# Exibir as primeiras linhas do novo dataframe
names(dados)

