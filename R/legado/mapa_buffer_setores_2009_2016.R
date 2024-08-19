# Pacotes  ----
library(tidyverse)
library(mapview)
library(ggmap)
library(sf)
library(dplyr)

# Bases ---
malha_2010 <- read_sf("Shapes/2010/53SEE250GC_SIR.shp")
brt <- read_sf("Shapes/BRT/estacoes_BRT.shp") |> slice(1:11)

distancia_tratamento <- 1500

## Estações BRT ----
estacoes_brt <- brt |> 
  mutate(lon_brt = st_coordinates(geometry)[,1],
         lat_brt = st_coordinates(geometry)[,2],) |> 
  select(ra = Name, lon_brt, lat_brt) |>
  st_drop_geometry() |> 
  filter(ra %in% c("Gama","Santa Maria")) 

estacoes_brt_sf <- estacoes_brt |> st_as_sf(coords = c("lon_brt", "lat_brt"), crs = 4326)

## Buffer Estações BRT ----
buffers <- st_buffer(estacoes_brt_sf, dist = distancia_tratamento)


# Função de mapear Setores Gama
setores_ped_gama <-function(ID){
  case_when(
    ID ==664~"012",
    ID ==645~"127",
    ID ==614~"055",
    ID ==643~"034",
    ID ==653~"119",
    ID ==659~"057",
    ID ==649~"067",
    ID %in%c(695,722)~"070",
    ID ==647~"078",
    ID %in%c(657,661)~"083",# ID %in% c(636, 637, 680, 679, 673) ~ "086", (Dúbio)
    ID %in%c(725,694)~"096",
    ID ==728~"112",
    ID %in%c(640,641)~"113",
    ID %in%c(668,659)~"114",
    ID %in%c(682,683)~"115",
    ID %in%c(666,765)~"116",
    ID %in%c(666,704)~"117",
    ID ==633~"118",
    ID ==663~"120",
    ID ==679~"129",
    ID ==680~"121",
    ID ==648~"122",
    ID %in%c(607,606,603,598)~"123",
    ID ==692~"124",
    ID %in%c(720,670)~"125",
    ID ==703~"126",
    ID %in%c(623,645)~"127",
    ID %in%c(708,770)~"130",
    ID ==652~"132",
    ID ==655~"133",
    ID ==662~"134",TRUE~NA_character_)}

# Função de mapear Setores Santa Maria
setores_ped_sm <-function(ID){
  case_when(
    ID %in%c(4146,4242)~"011",
    ID %in%c(2613,4243)~"018",
    ID %in%c(4154,4153)~"021",
    ID ==4158~"041",
    ID %in%2612~"045",
    ID %in%2611~"104",
    ID %in%c(4126,4136,4236)~"067",
    ID ==4203~"089",
    ID %in%c(4163,4164)~"095",
    ID ==4123~"097",
    ID ==297~"096",
    ID ==4131~"098",
    ID %in%c(4127,4128)~"099",
    ID %in%c(4151,4152)~"100",
    ID %in%c(4139,4140)~"101",
    ID %in%c(4142,4141)~"102",
    ID ==2617~"103",
    ID %in%c(4158,4251,4157)~"105",
    ID %in%c(4125,4126)~"106",
    ID %in%c(4241,4144)~"109",
    ID %in%c(4240,4239)~"110",
    ID %in%4122~"118",
    ID %in%4121~"108",
    ID %in%c(4189,4190)~"107",TRUE~NA_character_)}

# Tratamento ----
malha_2010 <- malha_2010 |> 
  mutate(setores_ped_gama = setores_ped_gama(ID)) |> 
  mutate(trat_gama = case_when(
    setores_ped_gama %in%c("118")~"Tratados",
    !is.na(setores_ped_gama)~"Controles",
    TRUE ~ NA_character_)) |> 
  mutate(setores_ped_sm = setores_ped_sm(ID)) |> 
  mutate(trat_sm = case_when(
    setores_ped_sm %in%c("089","107","096") ~ "Tratados",
    !is.na(setores_ped_sm) ~ "Controles",
    TRUE ~ NA_character_))

setores_sm <- malha_2010 |> 
  filter(!is.na(setores_ped_sm))


# Mapas ----
mapa_sm <- mapview(setores_sm,col.regions = "#6fb963", legend = TRUE)

setores_gama <- malha_2010 |> 
  filter(!is.na(setores_ped_gama))

mapa_gama <- mapview(setores_gama,col.regions = "#75a1c6", legend = TRUE) 

mapview(estacoes_brt_sf) + mapview(buffers,alpha.regions = 0.2) + mapa_gama + mapa_sm 

# saveRDS(malha_2010,"Rds/malha_2010_setores_ped.RDS")
# mapview(setores_sm,zcol = "trat_sm", legend = TRUE) + mapview(setores_gama,zcol = "trat_gama", legend = TRUE)
