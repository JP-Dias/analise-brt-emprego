#'*Prepara Microdados Nova PED*

# Pacotes ----
library(haven)
library(tidyverse)
library(dplyr)
library(labelled)

# Nova PED
nova_ped <- readRDS("Rds/nova_ped_empilhada.RDS")

# Seleção e renomeação das variáveis
dados_nova_ped <- nova_ped |> 
  mutate(
    informal = ifelse(pos == 2 | (pos %in% 5:6 & f280 == 2), 1, 0),  # Posição na ocupação 
    fem = case_when(c030 == 1 ~ 0, c030 == 2 ~ 1,TRUE ~ NA_integer_),
    trab_plano = ifelse(f202 == 5301, 1, 0),  # Local de trabalho principal 
    mora_mesma_ra = ifelse((m041 == 1 | m05a > 1), 1, 0),  # Residia na mesma RA 12 meses atrás ou sempre morou na RA
    ocupado = case_when(
      sit == 4 ~ 1,  # Situação ocupacional (V2009)
      sit %in% 1:3 ~ 0,
      TRUE ~ NA_integer_
    ),
    rend_bruto = ifelse(as.numeric(f481) %in% c(0,1e9,1e9 + 1), NA_integer_, as.numeric(f481)),  # Remuneração bruta (F481)
    rend_liquido = ifelse(as.numeric(f482) %in% c(0,1e9,1e9 + 1), NA_integer_, as.numeric(f482)),  # Remuneração líquida (F482)
    horas_trab = ifelse(as.numeric(f492) %in% c(0,1e3,1e3 + 1), NA_integer_, as.numeric(f492)),  # Horas Trabalhadas
    escol = case_when(
      inst %in% c(2,3) ~ "analf",
      inst == 4 ~ "fund_inc",
      inst == 5 ~ "fund_com",
      inst == 6 ~ "med_inc",
      inst == 7 ~ "med_com",
      inst == 8 ~ "sup_inc",
      inst == 9 ~ "sup_com",
      TRUE ~ NA_character_
    ),
    setor_atv = case_when(
      setor_cnae == 2000 ~ "indust",  
      setor_cnae == 3000 ~ "construc",
      setor_cnae == 4000 ~ "comerc",
      setor_cnae == 5000 ~ "servic",
      setor_cnae == 9999 ~ "outros",
      TRUE ~ NA_character_
    ),
    cor = case_when(
      c040 == 1 ~ "branca",  
      c040 == 2 ~ "preta",
      c040 == 3 ~ "parda",
      c040 == 4 ~ "amarela",
      c040 == 5 ~ "indígena",
      c040 == 6 ~ "ns",
      TRUE ~ NA_character_
    ),
    posicao_fam = case_when(
      c061 == 1 ~ "responsável",
      c061 %in% c(2,3) ~ "cônjuge",
      c061 == 4 ~ "filho",
      TRUE ~ "outros"
    )
  ) |> 
  select(
    ano, mes, # Ano e Mês
    conglom, # Conglomerado
    domic, # Domicílio
    ocupado, # Situação ocupacional
    rend_bruto, # Rendimento bruto
    rend_liquido, # Rendimento líquido
    informal, # Trabalhador Informal
    horas_trab, # Horas trabalhadas no trabalho principal
    trab_plano, # Dummy de trabalha na RA Plano Piloto
    setor_atv, # Setor de atividade CNAE
    escol, # Grau de instrução
    idade = c050, # Idade
    cor, # Cor
    fem, # Sexo
    mora_mesma_ra, # Mora na Messma RA 12 meses atrás
    pessoas = totmora, # Total de moradores no domicílio
    posicao_fam # Posição na família
  )


saveRDS(dados_nova_ped,"Rds/nova_ped_vars.RDS")

