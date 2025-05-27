library(tidyverse)
library(sf)
library(haven)
library(mapview)


dist_estacao <- read_sf("analysis/qgis/shapes/dist_estacao.shp")

dist_est <- dist_estacao |> 
  select(CD2000,CD2010,SUBDIST,ano,mes,domic,distance) |> 
  st_drop_geometry() |> 
  as.data.frame() |> 
  rename(dist_estacao = distance)

dist_linha_alim <- read_sf("analysis/qgis/shapes/dist_linha_alim/dist_linha_alim.shp")

dist_linha <- dist_linha_alim |> 
  select(CD2000,CD2010,SUBDIST,ano,mes,domic,distance) |> 
  st_drop_geometry() |> 
  as.data.frame() |> 
  rename(dist_linha = distance)


base_dist <- full_join(dist_linha,dist_est)

base_dist  |> nrow()

dados <- readRDS("analysis/dados/base.RDS")

dados <- dados |> rename(CD2000 = CODSETOR2000,CD2010 = CODSETOR2010,SUBDIST = NM_SUBDIST) |> 
  mutate(ano = as.double(ano))

dados_dist <- left_join(dados,unique(base_dist),by = c("ano","mes","domic","CD2000","CD2010","SUBDIST"))

write.csv2(dados_dist,"W:/GEFAPS/2024/BRT/analise-brt-emprego/analysis/dados/dados_dist.csv")
