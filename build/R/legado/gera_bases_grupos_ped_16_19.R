# Pacotes ----
library(haven)     
library(tidyverse) 
library(mapview)   
library(ggmap)     
library(sf)        

# Carrega a base de dados Nova PED já empilhada e seleciona colunas relevantes ----
nova_ped <- readRDS("rds/nova_ped_empilhada.RDS") |> 
  select(aamm, ano, mes, domic, conglom) |>
  unique() 

# Carrega a malha censitária de 2010 ----
malha_2010 <- read_sf("Shapes/2010/53SEE250GC_SIR.shp")

# Filtra a malha para incluir apenas os subdistritos de Santa Maria e Gama, em áreas urbanas ----
malha_gama_sm <- malha_2010 |> 
  filter(NM_SUBDIST %in% c("SANTA MARIA", "GAMA"), TIPO == "URBANO")

# Listagem dos arquivos SAV contendo a relação de domicílios por setores ----
arquivos_setor <- list.files(path = "relacao",
                             pattern = "\\.sav$", 
                             full.names = TRUE)   

# Função para ler e transformar arquivos SAV ----
ler_e_transformar <- function(arquivo) {
  print(arquivo) 
  temp <- read_sav(arquivo) 
  names(temp) <- str_to_lower(names(temp))
  temp 
}

# Construção da base de dados consolidada ----
bind_setor <- lapply(arquivos_setor, ler_e_transformar) |> 
  bind_rows() |>
  mutate(CD_GEOCODI = as.character(codsetor2010)) |> 
  rename(aamm = ano_mes, domic = num_domicilio) |> 
  select(-codsetor2010) 

# Cria a base final com dados geoespaciais e informações da PED (2016-2019) ----
malha_ped_16_19 <- nova_ped |> 
  select(aamm, domic) |>
  left_join(bind_setor, by = c("aamm", "domic")) |> 
  left_join(malha_2010, by = "CD_GEOCODI") |> 
  select(aamm, domic, ID, CD_GEOCODI, NM_SUBDIST, geometry) |> 
  na.omit()

# Alternativa de criação da base sem geometria ----
malha_ped_16_19 <- malha_2010 |>
  left_join(bind_setor, by = "CD_GEOCODI") |>
  left_join(nova_ped, by = c("aamm", "domic")) |> 
  select(ano, mes, domic, ID, CD_GEOCODI, NM_SUBDIST) |> 
  na.omit() |> 
  st_drop_geometry() 

# Cria uma base única de setores para PED 2016-2019 ----
setores_ped_16_19 <- malha_ped_16_19 |> 
  select(-c(ano, mes, domic)) |> 
  unique() 

# Salva os resultados em arquivos RDS ----
saveRDS(setores_ped_16_19, "rds/setores_censitarios_ped_16_19.RDS")

