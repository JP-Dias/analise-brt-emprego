# Pacotes ----
library(haven)
library(tidyverse)
library(mapview)
library(ggmap)
library(sf)

nova_ped <- readRDS("rds/nova_ped_empilhada.RDS") |> 
  select(aamm,ano, mes, domic, conglom) |>
  unique()

distancia_tratamento <- 1500

# Estações BRT ----
estacoes_brt <- readRDS("rds/geo_estacoes_brt.RDS")

## Buffer Estações BRT ----
buffers <- st_buffer(estacoes_brt, dist = distancia_tratamento)

# Malha 2010 ----
malha_2010 <- read_sf("Shapes/2010/53SEE250GC_SIR.shp")

malha_gama_sm <- malha_2010 |> 
  filter(NM_SUBDIST %in% c("SANTA MARIA","GAMA"),
         TIPO == "URBANO")

# Relação domicilios setores ----
arquivos_setor <- list.files(path = "relacao",
                             pattern = "\\.sav$",
                             full.names = TRUE)

# Função para ler arquivos
ler_e_transformar <- function(arquivo) {
  print(arquivo)
  temp <- read_sav(arquivo)
  names(temp) <- str_to_lower(names(temp))
  temp
}

# Construção da Base ----
# Lendo e empilhando os arquivos PED
bind_setor <- lapply(arquivos_setor, ler_e_transformar) |> 
  bind_rows() |> 
  mutate(CD_GEOCODI = as.character(codsetor2010)) |> 
  rename(aamm = ano_mes, domic = num_domicilio) |> 
  select(-codsetor2010)

malha_ped_16_19 <- nova_ped |> 
  select(aamm, domic) |> 
  left_join(bind_setor, by = c("aamm","domic")) |> 
  left_join(malha_2010, by = "CD_GEOCODI") |> 
  select(aamm,domic, ID, CD_GEOCODI, NM_SUBDIST, geometry) |> 
  na.omit()

malha_ped_16_19 <- malha_2010 |>
  left_join(bind_setor, by = "CD_GEOCODI") |> 
  left_join(nova_ped, by = c("aamm","domic")) |> 
  select(ano,mes,domic, ID, CD_GEOCODI, NM_SUBDIST, geometry) |> 
  na.omit()

setores_ped_16_19 <- malha_ped_16_19 |> select(-c(ano,mes,domic)) |> unique()

saveRDS(setores_ped_16_19,"rds/setores_censitarios_ped_16_19.RDS")


# Função de Classificação de Setores ----
classifica_setor <- function(setores, buffers) {
  setores_transformados <- st_transform(setores, st_crs(buffers))
  intersecao <- st_intersects(setores_transformados, buffers)
  
  setores_transformados$grupo <- apply(intersecao, 1, function(x) ifelse(any(x), "Tratamento", "Controle"))
  
  return(setores_transformados)
}

grupos_domic_ped_16_19 <- classifica_setor(malha_ped_16_19, buffers)

saveRDS(grupos_domic_ped_16_19,"rds/domic_setores_grupos_ped_16_19.RDS")



# mapa <- base |> select(domic,geometry) |> unique()
# 
# 
# setores_gama_np <- malha_2010 |> filter(CD_GEOCODI %in% bind_setor$CD_GEOCODI,NM_SUBDIST =="GAMA")
# setores_sm_np <- malha_2010 |> filter(CD_GEOCODI %in% bind_setor$CD_GEOCODI,NM_SUBDIST == "SANTA MARIA")
# 
# mapview(estacoes_brt_sf) + mapview(buffers,alpha.regions = 0.2) +
# mapview(setores_gama,col.regions = "#a2cae5", legend = TRUE) + 
# mapview(setores_sm,col.regions = "#8dd180", legend = TRUE) +
# mapview(setores_gama_np,col.regions = "#5277a4", legend = TRUE) + 
# mapview(setores_sm_np,col.regions = "#5ca150", legend = TRUE) 
# 
#   
#   
# setores_sm <- st_transform(setores_sm, st_crs(buffers))
# 
# 
# intersecao <- st_intersects(setores_sm, buffers, sparse = FALSE)
# 
# setores_sm$classificacao <- apply(intersecao, 1, function(x) ifelse(any(x), "Dentro", "Fora"))
# 
# 
# 
# classifica_setor <- function(setores, buffers) {
#   setores_transformados <- st_transform(setores, st_crs(buffers)) # Formato padrão
#   
#   intersecao <- st_intersects(setores_transformados, buffers, sparse = FALSE)
#   
#   setores_transformados$grupo <- apply(intersecao, 1, function(x) ifelse(any(x), "Tratamento", "Controle"))
#   
#   # Retornar os setores com a classificação
#   return(setores_transformados)
# }
# 
# resultado <- classifica_setor(setores_sm_np, buffers)
# 
# 
# plot(st_geometry(resultado), col = ifelse(resultado$grupo == "Tratamento", "green", "red"))
