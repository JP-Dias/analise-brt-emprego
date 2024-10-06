#'*Script de Configuração inicial e carregamento de funções*

# Pacotes ----
library(haven)
library(tidyverse)
library(data.table)
library(foreign)
library(readxl)
library(dplyr)
library(labelled)
library(survey)
library(srvyr)
library(mapview)
library(leaflet)
library(ggmap)     
library(sf) 
library(sidrar)
library(fastDummies)

options(scipen = 999)

build <- function(x){paste0("build/",x)}
analysis <- function(x){paste0("analysis/",x)}

# Funções de Leitura e Empilhamento de Arquivos ----
ler_e_transformar <- function(arquivo) {
  print(arquivo)
  temp <- read_sav(arquivo)
  names(temp) <- str_to_lower(names(temp)) # transformar nomes das colunas
  temp
}

ler_empilhar_arquivos <- function(arquivos, ler_funcao) {
  dados <- lapply(arquivos, function(arquivo) {
    print(arquivo)
    temp <- ler_funcao(arquivo)
    names(temp) <- str_to_lower(names(temp))
    return(temp)
  })
  bind_rows(dados)
}


# Funções de Mapeamento de Setores ----
## Função que mapeia IDs para setores PED em Gama
setores_ped_gama <- function(ID) {
  case_when(
    ID == 664 ~ "012",
    ID == 645 ~ "127",
    ID == 614 ~ "055",
    ID == 643 ~ "034",
    ID == 653 ~ "119",
    ID == 659 ~ "057",
    ID == 649 ~ "067",
    ID %in% c(695, 722) ~ "070",
    ID == 647 ~ "078",
    ID %in% c(657, 661) ~ "083",
    ID %in% c(725, 694) ~ "096",
    ID == 728 ~ "112",
    ID %in% c(640, 641) ~ "113",
    ID %in% c(668, 659) ~ "114",
    ID %in% c(682, 683) ~ "115",
    ID %in% c(666, 765) ~ "116",
    ID %in% c(666, 704) ~ "117",
    ID == 633 ~ "118",
    ID == 663 ~ "120",
    ID == 679 ~ "129",
    ID == 680 ~ "121",
    ID == 648 ~ "122",
    ID %in% c(607, 606, 603, 598) ~ "123",
    ID == 692 ~ "124",
    ID %in% c(720, 670) ~ "125",
    ID == 703 ~ "126",
    ID %in% c(623, 645) ~ "127",
    ID %in% c(708, 770) ~ "130",
    ID == 652 ~ "132",
    ID == 655 ~ "133",
    ID == 662 ~ "134",
    TRUE ~ NA_character_ # Caso não haja correspondência, retorna NA
  )
}

## Função que mapeia IDs para setores PED em Santa Maria
setores_ped_sm <- function(ID) {
  case_when(
    ID %in% c(4146, 4242) ~ "011",
    ID %in% c(2613, 4243) ~ "018",
    ID %in% c(4154, 4153) ~ "021",
    ID == 4158 ~ "041",
    ID %in% 2612 ~ "045",
    ID %in% 2611 ~ "104",
    ID %in% c(4126, 4136, 4236) ~ "067",
    ID == 4203 ~ "089",
    ID %in% c(4163, 4164) ~ "095",
    ID == 4123 ~ "097",
    ID == 297 ~ "096",
    ID == 4131 ~ "098",
    ID %in% c(4127, 4128) ~ "099",
    ID %in% c(4151, 4152) ~ "100",
    ID %in% c(4139, 4140) ~ "101",
    ID %in% c(4142, 4141) ~ "102",
    ID == 2617 ~ "103",
    ID %in% c(4158, 4251, 4157) ~ "105",
    ID %in% c(4125, 4126) ~ "106",
    ID %in% c(4241, 4144) ~ "109",
    ID %in% c(4240, 4239) ~ "110",
    ID %in% 4122 ~ "118",
    ID %in% 4121 ~ "108",
    ID %in% c(4189, 4190) ~ "107",
    TRUE ~ NA_character_ # Caso não haja correspondência, retorna NA
  )
}

# Função de Classificação de Setores ----
classifica_setor <- function(setores, buffers) {
  setores_transformados <- st_transform(setores, st_crs(buffers))
  intersecao <- st_intersects(setores_transformados, buffers)
  
  setores_transformados$grupo <- apply(intersecao, 1, function(x) ifelse(any(x), 1, 0))
  
  return(setores_transformados)
}


