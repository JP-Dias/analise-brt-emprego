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

base_sf <- base |> st_as_sf(coords = c("lon", "lat"), crs = 4326)

lista_subreg_gama <- base_sf |> 
  filter(reg == 242) |> 
  select(subreg) |> 
  st_drop_geometry() |> 
  unique() |> 
  pull()


base_sf_gama <- base_sf |> 
  filter(reg == 242,subreg == lista_subreg_gama[i]) |> 
  select(ano,mes,domic,conglom,subreg,endereco_comp) |> 
  mutate_geocode(endereco_comp, output = "latlon") |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

mapview(malha_2010, col = "red", col.regions = "grey", alpha.regions = 0.1, legend = FALSE,lwd = .5) +
  mapview(base_sf_gama, zcol = "subreg", col.regions = rainbow(length(unique(base_sf_gama$subreg))))


lista_base_sf_gama <-list()

for(i in lista_subreg_gama){
  
  lista_base_sf_gama[[i]]<- base_sf |> 
    filter(reg == 242, 
           subreg == lista_subreg_gama[i]) |> 
    select(ano, mes, domic, conglom,
           subreg, endereco_comp) |> 
    mutate_geocode(endereco_comp, 
                   output ="latlon") |> 
    st_as_sf(coords = c("lon","lat"), 
             crs = 4326)
}



# Gama ----
# subreg = 012, ID = 664
# subreg = 032, ID = 645
# subreg = 034, ID = 643
# subreg = 055, ID = 653
# subreg = 057, ID = 659
# subreg = 067, ID = 649
# subreg = 070, ID = 695, 722 , 789
# subreg = 078, ID = 647
# subreg = 083, ID = 657, 661
# subreg = 086, ID = 636, 637 (tratados), 680,679, 673
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

# Santa Maria --------
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

# Testes --------

lista_subreg_sm <- base_sf |> 
  filter(reg == 363) |> 
  select(subreg) |> 
  st_drop_geometry() |> 
  unique() |> 
  pull()


lista_base_sf_sm <-list()

for(i in lista_subreg_sm){
  
  lista_base_sf_sm[[i]]<- base_sf |> 
    filter(reg == 363, 
           subreg == lista_subreg_sm[i]) |> 
    select(ano, mes, domic, conglom,
           subreg, endereco_comp) |> 
    mutate_geocode(endereco_comp, 
                   output ="latlon") |> 
    st_as_sf(coords = c("lon","lat"), 
             crs = 4326)
}


base_sf_sm<- base_sf |> 
  filter(reg == 363,subreg == lista_subreg_sm[17]) |> 
  select(ano,mes,domic,conglom,subreg,endereco_comp) |> 
  mutate_geocode(endereco_comp, output = "latlon") |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

mapview(malha_2010, col = "red", col.regions = "grey", alpha.regions = 0.1, legend = FALSE,lwd = .5) +
  mapview(base_sf_sm, zcol = "subreg", col.regions = rainbow(length(unique(base_sf_sm$subreg))))


# Tratamentos --------
malha_2010 <- malha_2010 |> 
  mutate(
    setores_ped_gama = case_when(
      ID == 664 ~ "012",
      ID == 645 ~ "127",
      ID == 614 ~ "055",
      ID == 643 ~ "034",
      ID == 653 ~ "119",
      ID == 659 ~ "057",
      ID == 649 ~ "067",
      ID %in% c(695, 722) ~ "070",
      ID == 647 ~ "078",
      ID %in% c(657, 661) ~ "083",
      #ID %in% c(636, 637, 680, 679, 673) ~ "086", (Dúbio)
      ID %in% c(725, 694) ~ "096",
      ID == 728 ~ "112",
      ID %in% c(640, 641) ~ "113",
      ID %in% c(668, 659) ~ "114",
      ID %in% c(682, 683) ~ "115",
      ID %in% c(666, 765) ~ "116",
      ID %in% c(666, 704) ~ "117",
      ID == 633 ~ "118",
      ID == 663 ~ "120",
      ID == 679 ~ "129",
      ID == 680 ~ "121",
      ID == 648 ~ "122",
      ID %in% c(607, 606, 603, 598) ~ "123",
      ID == 692 ~ "124",
      ID %in% c(720, 670) ~ "125",
      ID == 703 ~ "126",
      ID %in% c(623, 645) ~ "127",
      ID %in% c(708, 770) ~ "130",
      ID == 652 ~ "132",
      ID == 655 ~ "133",
      ID == 662 ~ "134",
      TRUE ~ NA_character_
    )) |>
  mutate(trat_gama = case_when(setores_ped_gama %in% c("118") ~ "Tratados",
                               !is.na(setores_ped_gama) ~ "Controles",
                               TRUE ~ NA_character_)) |> 
  mutate(
    setores_ped_sm = case_when(
      ID %in% c(4146, 4242) ~ "011",
      ID %in% c(2613, 4243) ~ "018",
      ID %in% c(4154, 4153) ~ "021",
      ID == 4158 ~ "041",
      ID %in% 2612 ~ "045",
      ID %in% 2611 ~ "104",
      ID %in% c(4126, 4136, 4236) ~ "067",
      ID == 4203 ~ "089",
      ID %in% c(4163, 4164) ~ "095",
      ID == 4123 ~ "097",
      ID == 297 ~ "096",
      ID == 4131 ~ "098",
      ID %in% c(4127, 4128) ~ "099",
      ID %in% c(4151, 4152) ~ "100",
      ID %in% c(4139, 4140) ~ "101",
      ID %in% c(4142, 4141) ~ "102",
      ID == 2617 ~ "103",
      ID %in% c(4158, 4251, 4157) ~ "105",
      ID %in% c(4125, 4126) ~ "106",
      ID %in% c(4241, 4144) ~ "109",
      ID %in% c(4240, 4239) ~ "110",
      ID %in% 4122 ~ "118",
      ID %in% 4121 ~ "108",
      ID %in% c(4189, 4190) ~ "107",
      TRUE ~ NA_character_
    )) |> 
  mutate(trat_sm = case_when(setores_ped_sm %in% c("089","107","096") ~ "Tratados",
                             !is.na(setores_ped_sm) ~ "Controles",
                             TRUE ~ NA_character_))

setores_sm <- malha_2010 |> 
  filter(!is.na(setores_ped_sm))

mapa_sm <- mapview(setores_sm,col.regions = "#6fb963", legend = TRUE)


setores_gama <- malha_2010 |> 
  filter(!is.na(setores_ped_gama))

mapa_gama <- mapview(setores_gama,col.regions = "#75a1c6", legend = TRUE) 

mapview(estacoes_brt_sf) + mapview(buffers,alpha.regions = 0.2) + mapa_gama + mapa_sm 

saveRDS(malha_2010,"Rds/malha_2010_setores_ped.RDS")


mapview(setores_sm,zcol = "trat_sm", legend = TRUE) +
  mapview(setores_gama,zcol = "trat_gama", legend = TRUE)
