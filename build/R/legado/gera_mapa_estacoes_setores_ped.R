# Pacotes ----
library(tidyverse)
library(mapview)
library(ggmap)
library(sf)
library(dplyr)


# Estações BRT ----
distancia_tratamento <- 1500
estacoes_brt <- readRDS("rds/geo_estacoes_brt.RDS")

## Buffer Estações BRT ----
buffers <- st_buffer(estacoes_brt, dist = distancia_tratamento)


st_16_19 <- readRDS("rds/setores_censitarios_ped_16_19.RDS") |> filter(NM_SUBDIST %in% c("GAMA","SANTA MARIA")) |> mutate(ped = "Nova PED")
st_09_16 <- readRDS("rds/setores_censitarios_ped_09_16.RDS") |> mutate(ped = "PED")

mapview(st_16_19,col.regions = "#a2cae5", col = "#a2cae5") + 
mapview(st_09_16,col.regions = "#5277a4", col = "#5277a4") +
mapview(buffers, col.regions = "#f2cb64",col = "#f2cb64",alpha.regions = 0.2) +
mapview(estacoes_brt,col.regions = "#f0c74b")  

