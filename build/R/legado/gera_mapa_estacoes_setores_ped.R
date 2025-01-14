# Pacotes ----
library(tidyverse)
library(mapview)
library(ggmap)
library(sf)
library(dplyr)


# Estações BRT ----
distancia_tratamento <- 3000
estacoes_brt <- readRDS(build("rds/geo_estacoes_brt.RDS"))

## Buffer Estações BRT ----
buffers <- st_buffer(estacoes_brt, dist = distancia_tratamento)


st_16_19 <- readRDS(build("rds/setores_censitarios_ped_16_19.RDS")) |> filter(NM_SUBDIST %in% c("GAMA","SANTA MARIA")) |> mutate(ped = "Nova PED")
st_09_16 <- readRDS(build("rds/setores_censitarios_ped_09_16.RDS")) |> mutate(ped = "PED")

mapview(st_16_19,col.regions = "#5277a4", col = "#5277a4") + 
mapview(st_09_16,col.regions = "#5277a4", col = "#5277a4") +
mapview(st_buffer(estacoes_brt, dist = 1500), col.regions = "#f2cb64",col = "#f2cb64",alpha.regions = 0.1,lwd =3) +
mapview(st_buffer(estacoes_brt, dist = 2500), col.regions = "#f2cb64",col = "#f2cb64",alpha.regions = 0.1,lwd =3) +
mapview(st_buffer(estacoes_brt, dist = 3500), col.regions = "#f2cb64",col = "#f2cb64",alpha.regions = 0.1,lwd =3) +
mapview(st_buffer(estacoes_brt, dist = 4500), col.regions = "#f2cb64",col = "#f2cb64",alpha.regions = 0.1,lwd =3) +
mapview(st_buffer(estacoes_brt, dist = 4500), col.regions = "#f2cb64",col = "#f2cb64",alpha.regions = 0.1,lwd =3) +
mapview(st_buffer(estacoes_brt, dist = 5500), col.regions = "#f2cb64",col = "#f2cb64",alpha.regions = 0.1,lwd =3) +
mapview(st_buffer(estacoes_brt, dist = 6500), col.regions = "#f2cb64",col = "#f2cb64",alpha.regions = 0.1,lwd =3) +
mapview(estacoes_brt,col.regions = "#f0c74b")  

