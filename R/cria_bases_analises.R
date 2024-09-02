# Pacotes ----
library(haven)
library(tidyverse)
library(dplyr)
library(labelled)

# Lê dados ----

# Malhas
source("R/altera_malha_bases.R")

# Microdados
ped <- readRDS("Rds/ped_vars.RDS")
nova_ped <- readRDS("Rds/nova_ped_vars.RDS")

# Estações BRT ----
estacoes_brt <- readRDS("rds/geo_estacoes_brt.RDS")

## Buffer Estações BRT ----
buffer15 <- st_buffer(estacoes_brt, dist = 1500)
buffer20 <- st_buffer(estacoes_brt, dist = 2000)


# Função de Classificação de Setores ----
classifica_setor <- function(setores, buffers) {
  setores_transformados <- st_transform(setores, st_crs(buffers))
  intersecao <- st_intersects(setores_transformados, buffers)
  
  setores_transformados$grupo <- apply(intersecao, 1, function(x) ifelse(any(x), "Tratamento", "Controle"))
  
  return(setores_transformados)
}

classifica_setor(join_ped_09_16_malha_2000,buffer15)
classifica_setor(join_ped_16_19_malha_2000,buffer15)


ped |> left_join(join_ped_09_16_malha_2000)

nova_ped |> left_join(join_ped_16_19_malha_2000)

mapview(buffer15,alpha.regions = .2) + mapview(buffer20,alpha.regions = .2) + mapview(estacoes_brt)

# Função de Classificação de Setores ----
classifica_setor <- function(setores, buffers) {
  # Corrigir geometrias inválidas
  setores <- st_make_valid(setores)
  buffers <- st_make_valid(buffers)
  
  # Transformar para o mesmo CRS
  setores_transformados <- st_transform(setores, st_crs(buffers))
  
  # Interseção entre setores e buffers
  intersecao <- st_intersects(setores_transformados, buffers)
  
  # Classificação dos setores
  setores_transformados$grupo <- apply(intersecao, 1, function(x) ifelse(any(x), "Tratamento", "Controle"))
  
  return(setores_transformados)
}
