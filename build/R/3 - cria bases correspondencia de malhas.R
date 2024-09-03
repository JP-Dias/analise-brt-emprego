#'*Scrip que cria as infomações de correspondência entre os domicílios PED e Setores Censitários da Malha de 2010*

source("config.R")

# Carregamento de Dados ----
malha_2010 <- read_sf(build("Shapes/2010/53SEE250GC_SIR.shp"))

# PED ----

## Carrega e filtra a base PED para os conglomerados de interesse ----
ped <- readRDS(build("Rds/ped_empilhada.RDS")) |>
  mutate(conglom = substr(conglom, 1, 6)) |> 
  filter(substr(conglom, 1, 3) %in% c(242, 363)) |> 
  select(ano, mes, domic, conglom) |> 
  unique() 

## Tratamento das Bases ----
malha_2010 <- malha_2010 |> 
  mutate(
    setores_ped_gama = setores_ped_gama(ID),
    setores_ped_sm = setores_ped_sm(ID)
  )

## Filtra e transforma a malha com base nos setores mapeados e na PED ----
malha_ped_09_16 <- malha_2010 |>
  filter(!is.na(setores_ped_gama) | !is.na(setores_ped_sm)) |> 
  mutate(
    conglom = ifelse(!is.na(setores_ped_gama),
                     paste0(242, setores_ped_gama), 
                     paste0(363, setores_ped_sm))   
  ) |>
  select(conglom, ID, CD_GEOCODI, NM_SUBDIST) |>
  left_join(ped) |> 
  unique() |> 
  st_drop_geometry() 


# Nova PED ----

## Carrega a base de dados Nova PED já empilhada e seleciona colunas relevantes ----
nova_ped <- readRDS(build("rds/nova_ped_empilhada.RDS")) |> 
  select(aamm, ano, mes, domic, conglom) |>
  unique() 


## Filtra a malha para incluir apenas os subdistritos de Santa Maria e Gama, em áreas urbanas ----
malha_gama_sm <- malha_2010 |> 
  filter(NM_SUBDIST %in% c("SANTA MARIA", "GAMA"), TIPO == "URBANO")

## Listagem dos arquivos SAV contendo a relação de domicílios por setores ----
arquivos_setor <- list.files(path = build("relacao"),
                             pattern = "\\.sav$", 
                             full.names = TRUE)   

## Construção da base de dados consolidada ----
bind_setor <- lapply(arquivos_setor, ler_e_transformar) |> 
  bind_rows() |>
  mutate(CD_GEOCODI = as.character(codsetor2010)) |> 
  rename(aamm = ano_mes, domic = num_domicilio) |> 
  select(-codsetor2010) 

## Filtra e transforma a malha com base nos setores mapeados e na Nova PED ----
malha_ped_16_19 <- malha_2010 |>
  left_join(bind_setor, by = "CD_GEOCODI") |>
  left_join(nova_ped, by = c("aamm", "domic")) |> 
  select(ano, mes, domic, ID, CD_GEOCODI, NM_SUBDIST) |> 
  na.omit() |> 
  st_drop_geometry() 
