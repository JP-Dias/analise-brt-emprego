library(haven)
library(tidyverse)
library(data.table)
library(foreign)
library(readxl)
library(dplyr)

source("R/gera_bases_grupos_ped_09_16.R")
source("R/gera_bases_grupos_ped_16_19.R")

teste <- read_sav("relacao/malhas2000e2010/Compatibiliza DF FINAL.sav")

malha_2000 <- read_sf("Shapes/2000/5300108.SHP")
malha_2010 <- read_sf("Shapes/2010/53SEE250GC_SIR.shp")

malha_2000 <- st_set_crs(malha_2000, 32723)
malha_2000 <- st_transform(malha_2000, 32723)


malha_2000 <- st_transform(malha_2000, 4674)  # Reprojetando para SIRGAS 2000
malha_2010 <- st_transform(malha_2010, 4674)  # Reprojetando para SIRGAS 2000


# Calculando a diferença de deslocamento
dx <- 0.0007  # Defina o valor de deslocamento no eixo x
dy <- -0.0003  # Defina o valor de deslocamento no eixo y


# Aplicando a translação nas coordenadas da malha
malha_2000_translated <- st_geometry(malha_2000) + c(dx, dy)

# Atualizando a geometria da malha
st_geometry(malha_2000) <- malha_2000_translated

# Reatribuindo o CRS original à malha
st_crs(malha_2000) <- st_crs(malha_2010)  # ou use o CRS original da malha_2000

mapview(malha_2010,col.regions = "grey" ,col = "blue",alpha.regions = 0.2) + mapview(malha_2000,col.regions = "grey" ,col = "red",alpha.regions = 0.2)


teste_join_09_16 <- grupos_domic_ped_09_16 |> 
  left_join(
    teste |> mutate(CD_GEOCODI = as.character(CODSETOR2010)),by = "CD_GEOCODI"
  )

mapview(teste_join_09_16 |> select(CODSETOR2000))
mapview(teste_join_09_16 |> select(CODSETOR2010))

teste_join_16_19 <- grupos_domic_ped_16_19 |> 
  left_join(
    teste |> mutate(CD_GEOCODI = as.character(CODSETOR2010)),by = "CD_GEOCODI"
  )

mapview(teste_join_16_19 |> filter(NM_SUBDIST %in% c("GAMA", "SANTA MARIA")) |> select(CODSETOR2000), col.regions = "grey" ,col = "blue",alpha.regions = 0.001) +
mapview(teste_join_16_19 |> filter(NM_SUBDIST %in% c("GAMA", "SANTA MARIA")) |> select(CODSETOR2010), col.regions = "grey" ,col = "red",alpha.regions = 0.001 )

