# Pacotes ----
library(haven)
library(tidyverse)
library(mapview)
library(ggmap)
library(sf)

brt <- read_sf("Shapes/BRT/estacoes_BRT.shp") |> slice(1:11)

# Estações BRT ----
estacoes_brt <- brt |> 
  mutate(lon_brt = st_coordinates(geometry)[,1],
         lat_brt = st_coordinates(geometry)[,2],) |> 
  select(ra = Name, lon_brt, lat_brt) |>
  st_drop_geometry() |> 
  filter(ra %in% c("Gama","Santa Maria")) 

estacoes_brt_sf <- estacoes_brt |> st_as_sf(coords = c("lon_brt", "lat_brt"), crs = 4326)

## Salva Objeto ----
saveRDS(estacoes_brt_sf,"rds/geo_estacoes_brt.RDS")
