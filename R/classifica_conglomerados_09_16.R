# Pacotes  ----
library(tidyverse)
library(mapview)
library(ggmap)
library(sf)
library(dplyr)

# Bases ---
ped <- readRDS("Rds/ped_empilhada.RDS")
geo_google <- readRDS("Rds/geo_google.RDS")
malha_2010 <- read_sf("Shapes/2010/53SEE250GC_SIR.shp")

# API Google
register_google(Sys.getenv("api_google"))

# Lista de Conglomerados
conglom <- ped |> select(domic, conglom, ano, mes) |> unique()

# Lista de Endereços
google <- geo_google |> select(domic,ano,mes,endereco_comp = endereco_min_comp,lon,lat) 

base <- left_join(conglom,google, by = c("domic","ano", "mes")) |> na.omit()

base$reg <- substr(base$conglom,1,3)
base$subreg <- substr(base$conglom,4,6)


# Classificação Gama ----
lista_subreg_gama <- base |> 
  filter(reg == 242) |> 
  select(subreg) |> 
  unique() |> 
  pull()

lista_base_sf_gama <-list()

for(i in 1:length(lista_subreg_gama)){
  
  lista_base_sf_gama[[i]] <- base |> 
    filter(reg == 242, 
           subreg == lista_subreg_gama[i]) |> 
    select(ano, mes, domic, conglom,
           subreg, endereco_comp)
}

base_sf_gama <- lista_base_sf_gama[[1 # modificar para realizar a classificação manualmente
]] |> 
  mutate_geocode(endereco_comp, output = "latlon") |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

mapview(malha_2010 |> filter(NM_SUBDIST == "GAMA"), col = "red", col.regions = "grey", alpha.regions = 0.1, legend = FALSE,lwd = .5) +
  mapview(base_sf_gama, zcol = "subreg")

## Gama ----
# subreg = 012, ID = 664
# subreg = 032, ID = 645
# subreg = 034, ID = 643
# subreg = 055, ID = 653
# subreg = 057, ID = 659
# subreg = 067, ID = 649
# subreg = 070, ID = 695, 722 , 789
# subreg = 078, ID = 647
# subreg = 083, ID = 657, 661
######## subreg = 086, ID = 636, 637 (tratados), 680,679, 673 (setor descartado por ser dúbio)
# subreg = 096, ID = 725, 694
# subreg = 112, ID = 728
# subreg = 113, ID = 640, 641
# subreg = 114, ID = 668, 659
# subreg = 115, ID = 682, 683
# subreg = 116, ID = 666, 765
# subreg = 117, ID = 666, 704
# subreg = 118, ID = 633
# subreg = 119, ID = 653
# subreg = 120, ID = 663
# subreg = 121, ID = 680
# subreg = 122, ID = 648
# subreg = 123, ID = 607,606,603, 598
# subreg = 124, ID = 692
# subreg = 125, ID = 720,670
# subreg = 126, ID = 703
# subreg = 127, ID = 623 ou 645
# subreg = 129, ID = 680
# subreg = 130, ID = 708,770
# subreg = 131, ID = 695
# subreg = 132, ID = 652
# subreg = 133, ID = 655
# subreg = 134, ID = 662


# Classificação Santa Maria ----
lista_subreg_sm <- base |> 
  filter(reg == 363) |> 
  select(subreg) |> 
  unique() |> 
  pull()


lista_base_sf_sm <-list()

for(i in 1:length(lista_subreg_sm)){
  
  lista_base_sf_sm[[i]] <- base |> 
    filter(reg == 363, 
           subreg == lista_subreg_sm[i]) |> 
    select(ano, mes, domic, conglom,
           subreg, endereco_comp)
}

base_sf_sm <- lista_base_sf_sm[[1 # modificar para realizar a classificação manualmente
]] |> 
  mutate_geocode(endereco_comp, output = "latlon") |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

mapview(malha_2010 |> filter(NM_SUBDIST == "SANTA MARIA"), col = "red", col.regions = "grey", alpha.regions = 0.1, legend = FALSE,lwd = .5) +
  mapview(base_sf_sm, zcol = "subreg")


## Santa Maria --------
# subreg = 242012, ID = 664
# subreg = 242032, ID = 645
# subreg = 242034, ID = 643
# subreg = 242055, ID = 653
# subreg = 242057, ID = 659
# subreg = 242067, ID = 649
# subreg = 242070, ID = 695, 722 , 789
# subreg = 242078, ID = 647
# subreg = 242083, ID = 657, 661
# subreg = 242086, ID = 636, 637 (tratados), 680,679, 673
# subreg = 242096, ID = 725, 694
# subreg = 242112, ID = 728
# subreg = 242113, ID = 640, 641
# subreg = 242114, ID = 668, 659
# subreg = 242115, ID = 682, 683
# subreg = 242116, ID = 666, 765
# subreg = 242117, ID = 666, 704
# subreg = 242118, ID = 633
# subreg = 242119, ID = 653
# subreg = 242120, ID = 663
# subreg = 242121, ID = 680
# subreg = 242122, ID = 648
# subreg = 242123, ID = 607,606,603, 598
# subreg = 242124, ID = 692
# subreg = 242125, ID = 720,670
# subreg = 242126, ID = 703
# subreg = 242127, ID = 623
# subreg = 242129, ID = 680
# subreg = 242130, ID = 708,770
# subreg = 242131, ID = 695
# subreg = 242132, ID = 652
# subreg = 242133, ID = 655
# subreg = 242134, ID = 662

