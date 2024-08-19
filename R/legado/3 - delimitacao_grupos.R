# Pacotes  ----

library(tidyverse)
library(mapview)
library(sf)
library(dplyr)
library(geosphere)

# Bases ----
brt <- read_sf("Shapes/BRT/estacoes_BRT.shp") |> slice(1:11)
geo_google <- readRDS("Rds/geo_google.RDS")
end <- readRDS("Rds/base_enderecos_ped.RDS")

distancia_tratamento <- 1500

# Tratamentos ----

## Tratamento Estações BRT ----
estacoes_brt <- brt |> 
  mutate(lon_brt = st_coordinates(geometry)[,1],
         lat_brt = st_coordinates(geometry)[,2],) |> 
  select(ra = Name, lon_brt, lat_brt) |>
  st_drop_geometry() |> 
  filter(ra %in% c("Gama","Santa Maria")) 

estacoes_brt_sf <- estacoes_brt |> st_as_sf(coords = c("lon_brt", "lat_brt"), crs = 4326)


## Buffer Estações BRT ----
buffers <- st_buffer(estacoes_brt_sf, 
                     dist = distancia_tratamento)

## Base de endereços
geo_google <- geo_google |> 
  # Filtra Erros de Geo
  filter(lon > -50,
         lat > -20,
         domic != 54112) |>
  # Seleciona vars de interesse
  select(ano, mes, domic, ra, 
         endereco_min_comp,
         lon_domic = lon,
         lat_domic = lat) 

geo_google_sf <- geo_google |> st_as_sf(coords = c("lon_domic", "lat_domic"), crs = 4326)

# Mapa dos pontos e buffers
mapview(geo_google_sf) + mapview(estacoes_brt_sf) + mapview(buffers,alpha.regions = 0.2)


# Base para cálculo de distâncias ----
base <- left_join(estacoes_brt,geo_google)

base$distancia <- mapply(function(lat_domic, lon_domic, lat_brt, lon_brt) {
  distHaversine(
    c(lon_domic, lat_domic),c(lon_brt, lat_brt)
                )
}, base$lat_domic, base$lon_domic, base$lat_brt, base$lon_brt)


