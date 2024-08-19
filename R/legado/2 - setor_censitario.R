library(tidyverse)
library(mapview)
library(sf)
library(dplyr)


ped <- readRDS("Rds/ped_empilhada.RDS")
end <- readRDS("Rds/base_enderecos_ped.RDS")
end_geo <- readRDS("Rds/geo_google.RDS")
malha_2010 <- read_sf("Shapes/2010/53SEE250GC_SIR.shp")
malha_2000 <- read_sf("Shapes/2000/5300108.shp")

testes <- left_join(ped,end_geo,by = c("ano","mes","domic")) |> 
  left_join(end) |> 
  select(domic,conglom,
         ano,mes,ra,upa,
         codra,grupo,
         endereco_min,lon,lat) |> 
  na.omit() |> 
  filter(lon > -50,
         lat > -20,
         domic != 54112) |> 
  unique()

testes$dois <- substr(testes$conglom,1,2)
testes$tres <- substr(testes$conglom,1,3)
testes$meio <- substr(testes$conglom,4,6)

testes_gama <- testes |> 
  filter(dois == 24,
         ra == "Gama")

testes_sf <- testes_gama |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)  # Definindo o CRS como WGS 84

mapview(malha_2010, col = "red", col.regions = "grey", alpha.regions = 0.1, legend = FALSE,lwd = 2) +
  mapview(testes_sf, zcol = "meio", col.regions = rainbow(length(unique(testes_sf$meio))))

testes_sm <- testes |> 
  filter(dois == 36,
         ra == "Santa Maria")

testes_sf <- testes_sm |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)  # Definindo o CRS como WGS 84

# Crie o mapa com mapview
mapview(malha_2010, col = "red", col.regions = "grey", alpha.regions = 0.1, legend = FALSE,lwd = 2) +
mapview(testes_sf, zcol = "meio", col.regions = rainbow(length(unique(testes_sf$meio)))) 


testes_sm <- testes |> 
  filter(dois == 36,
         ra == "Santa Maria")

table(testes_sm$conglom)

table(testes_gama$conglom)
