

source("config.R")

# Carregamento de Dados ----
malha_2010 <- read_sf(build("Shapes/2010/53SEE250GC_SIR.shp"))

# Carrega e filtra a base PED para os conglomerados de interesse
ped <- readRDS(build("Rds/ped_empilhada.RDS")) |>
  mutate(conglom = substr(conglom, 1, 6)) |> 
  filter(substr(conglom, 1, 3) %in% c(242, 363)) |> 
  select(ano, mes, domic, conglom) |> 
  unique() 

# Tratamento das Bases ----
malha_2010 <- malha_2010 |> 
  mutate(
    setores_ped_gama = setores_ped_gama(ID),
    setores_ped_sm = setores_ped_sm(ID)
  )

# Filtra e transforma a malha com base nos setores mapeados e na PED
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

# Cria uma base única de setores para PED 2009-2016 ----
setores_ped_09_16 <- malha_ped_09_16 |> 
  select(-c(conglom, ano, mes, domic)) |> 
  unique() 

# Salva o dataset consolidado de setores ----
saveRDS(setores_ped_09_16,build("Rds/setores_censitarios_ped_09_16.RDS"))

