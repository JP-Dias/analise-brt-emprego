# Pacotes -----
library(tidyverse)
library(tidygeocoder)
library(mapview)
library(sf)
library(furrr)
library(ggmap)

# Base Bruta ----
lista_end <- readRDS("Rds/base_enderecos_ped.RDS")

# API do Google Maps
register_google(Sys.getenv("api_google"))

# Função para ajustar endereços
ajustar_endereco <- function(endereco) {
  endereco |>
    str_trim() |>
    str_to_upper() |>
    str_squish() |>
    # Remover palavras indesejadas
    str_replace_all("FUNDOS","FUNDO") |> 
    str_replace_all("ESQUERD(O|A)|ESQUEDA|BAIX(O|A)|ALTOS|DIREIT(O|A)|DIRIREITA|FUNDOs|FUNDO|MEIO|FRE.ESQ|FUN ESQ|TERREO|ALT(O|A)|FU|LAT\\.ESQ\\.|
                            SEG\\. ARROLAGEM|S/N|SEG. MOR\\.|SN SEG.MOR\\.|N°|SEG A ARROLAGEM P/|VER MAPA 1|EM SEQ A|-|", "") |> 
    str_replace_all("FUNDOS|FRENTE|LATERAL|BAIXO\\s*ESQUERDO|LATERAL\\s*DIREITO", "") |>
    # Substituir abreviações
    str_replace_all("APTO", " APT ") |>
    str_replace_all("APT", " APT ") |>
    str_replace_all("\\bCS\\b|\\bCS\\.\\b", "CASA") |>
    str_replace_all("\\bLT\\b|\\bLT\\.\\b", "LOTE") |>
    str_replace_all("\\bQD\\b|\\bQD\\.\\b", "QUADRA") |>
    str_replace_all("\\bCJ\\b|\\bCJ\\.\\b|\\bCONJ\\b|\\bCONJ\\.\\b", "CONJUNTO") |>
    str_replace_all("\\bAPT\\b|\\bAPT\\.\\b|\\bAPTO\\b|\\bAPTº\\b", "APARTAMENTO") |>
    str_replace_all("ST\\s*L\\s*Q", "SETOR LESTE QUADRA") |>
    str_replace_all("ST\\s*S\\s*Q", "SETOR SUL QUADRA") |>
    # Remover detalhes indesejados
    str_replace_all("SEGUE\\s*ARROLAGEM|DESENHO\\s*\\d+\\s*ART\\s*\\d+|FRE\\s*ALOTEA", "") |>
    str_replace_all("CASA"," CASA ") |> 
    str_replace_all("LOTE"," LOTE ") |> 
    str_replace_all("QUADRA"," QUADRA ") |> 
    str_replace_all("CONJUNTO"," CONJUNTO ") |> 
    str_replace_all("APARTAMENTO"," APARTAMENTO ") |> 
    str_replace_all("\\.","") |> 
    str_replace_all("SEG MOR|SEG MORADOR","") |>
    str_replace_all("°", "") |>
    # Normalizar espaços
    str_replace_all("\\s+", " ") |>
    str_trim()
}

# Ajusta endereços ----
lista_geocode <- lista_end |>
  select(ano,mes,domic,ra,endereco) |>
  mutate(endereco_comp = endereco |> 
           ajustar_endereco() |> 
           str_to_title() |> 
           paste(ra,"Distrito Federal",sep = ", ")
         ) |> 
  filter(ra %in% c("Gama","Santa Maria"))

lista <- lista_geocode

# Georreferenciamento ----

# Função que georreferencia os endereços
geocode_google <- function(data) {
  data |> 
    mutate_geocode(endereco_comp, output = "more")
}

# Listas para armazenar resultados
resultado_google <- list()

# Georreferenciamento ano a ano
anos <- unique(lista$ano)

# Loop que georreferencia os endereços por ano (demorado)
for (i in anos) {
  print(paste("Processando ano:", i))

  resultado_google[[i]] <- geocode_google(lista |> filter(ano == i))
}

# Combina os resultados de todos os anos
final_google <- bind_rows(resultado_google)

# Checar resultado
final_google |> 
na.omit() |> 
  st_as_sf(
    coords = c("lon","lat"),
    crs = 4326
  ) |> 
  mapview()

# Salva Resultado
saveRDS(final_google,"Rds/geo_google.RDS")

