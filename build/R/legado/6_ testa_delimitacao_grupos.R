# Pacotes  ----

library(tidyverse)
library(mapview)
library(ggmap)
library(sf)
library(dplyr)

# Bases ---
ped <- readRDS("Rds/ped_empilhada.RDS")

ped$reg <- substr(ped$conglom,1,3) |> as.character()
ped$subreg <- substr(ped$conglom,4,6) |> as.character()

ped_gama <- ped |> filter(reg == 242)

ped_sm <- ped |> filter(reg == 363)


ped_gama <- ped_gama |> 
  mutate(trat = ifelse(subreg %in% c("118"),"Tratados","Controles"))

ped_sm <- ped_sm |> 
  mutate(trat = ifelse(subreg %in% c("089","107","096"),"Tratados","Controles"))

table(ped_gama$trat,ped_gama$ano)

table(ped_sm$trat,ped_sm$ano)
