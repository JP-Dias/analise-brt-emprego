# Pacotes ----
library(haven)
library(tidyverse)
library(dplyr)
library(labelled)

# Lê dados ----

# Malhas
source("R/altera_malha_bases.R")

# Microdados
ped <- readRDS("Rds/ped_vars.RDS")
nova_ped <- readRDS("Rds/nova_ped_vars.RDS")

# Estações BRT ----
estacoes_brt <- readRDS("rds/geo_estacoes_brt.RDS")

## Buffer Estações BRT ----
buffer15 <- st_buffer(estacoes_brt, dist = 1500)
buffer20 <- st_buffer(estacoes_brt, dist = 2000)

ped |> left_join(join_ped_09_16_malha_2000)
nova_ped |> left_join(join_ped_16_19_malha_2000)

