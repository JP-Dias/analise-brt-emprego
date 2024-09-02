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
buffer25 <- st_buffer(estacoes_brt, dist = 2500)
buffer30 <- st_buffer(estacoes_brt, dist = 3000)


# Função de Classificação de Setores ----
classifica_setor <- function(setores, buffers) {
  setores_transformados <- st_transform(setores, st_crs(buffers))
  intersecao <- st_intersects(setores_transformados, buffers)
  
  setores_transformados$grupo <- apply(intersecao, 1, function(x) ifelse(any(x), "Tratamento", "Controle"))
  
  return(setores_transformados)
}

# Função de Classificação de Setores ----
classifica_setor <- function(setores, buffers) {
  # Corrigir geometrias inválidas ç6
  setores <- st_make_valid(setores)
  buffers <- st_make_valid(buffers)
  
  # Transformar para o mesmo CRS
  setores_transformados <- st_transform(setores, st_crs(buffers))
  
  # Interseção entre setores e buffers
  intersecao <- st_intersects(setores_transformados, buffers)
  
  # Classificação dos setores
  setores_transformados$grupo <- apply(intersecao, 1, function(x) ifelse(any(x), 1, 0))
  
  return(setores_transformados)
}

setores_validos <- st_make_valid(join_ped_09_16_malha_2000)
setores_validos <- st_make_valid(join_ped_16_19_malha_2000)


sf_use_s2(FALSE)
grupos_15_ped_09_16 <- classifica_setor(join_ped_09_16_malha_2000, buffer15)
grupos_20_ped_09_16 <- classifica_setor(join_ped_09_16_malha_2000, buffer20)
grupos_25_ped_09_16 <- classifica_setor(join_ped_09_16_malha_2000, buffer25)
grupos_30_ped_09_16 <- classifica_setor(join_ped_09_16_malha_2000, buffer30)

grupos_15_ped_16_19 <- classifica_setor(join_ped_16_19_malha_2000, buffer15)
grupos_20_ped_16_19 <- classifica_setor(join_ped_16_19_malha_2000, buffer20)
grupos_25_ped_16_19 <- classifica_setor(join_ped_16_19_malha_2000, buffer25)
grupos_30_ped_16_19 <- classifica_setor(join_ped_16_19_malha_2000, buffer30)
sf_use_s2(TRUE) 

join_09_16 <- ped |>
  mutate(conglom = as.double(substr(conglom,1,6))) |> 
  left_join(grupos_15_ped_09_16 |> rename(grupo_15 = grupo) |> st_drop_geometry()) |> 
  left_join(grupos_20_ped_09_16 |> rename(grupo_20 = grupo) |> st_drop_geometry()) |> 
  left_join(grupos_25_ped_09_16 |> rename(grupo_25 = grupo) |> st_drop_geometry()) |> 
  left_join(grupos_30_ped_09_16 |> rename(grupo_30 = grupo) |> st_drop_geometry()) |> 
  filter(!is.na(CODSETOR2000)) |> unique()

join_16_19 <- nova_ped |> 
  left_join(grupos_15_ped_16_19 |> rename(grupo_15 = grupo) |> st_drop_geometry()) |> 
  left_join(grupos_20_ped_16_19 |> rename(grupo_20 = grupo) |> st_drop_geometry()) |> 
  left_join(grupos_25_ped_16_19 |> rename(grupo_25 = grupo) |> st_drop_geometry()) |> 
  left_join(grupos_30_ped_16_19 |> rename(grupo_30 = grupo) |> st_drop_geometry()) |> 
  filter(!is.na(CODSETOR2000)) |> unique()

base <- rbind(
  join_09_16,
  join_16_19
)




mapview(grupos_30, zcol = "grupo") + mapview(buffer30, col.regions = "#fde333", alpha.regions = .1)  + mapview(estacoes_brt, col.regions = "#fde333")

ped |> left_join(join_ped_09_16_malha_2000)

nova_ped |> left_join(join_ped_16_19_malha_2000)

mapview(buffer15,alpha.regions = .2) + mapview(buffer20,alpha.regions = .2) + mapview(estacoes_brt)


