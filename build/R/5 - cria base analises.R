
source("config.R")
source(build("R/4 - prepara malha.R"))

# Microdados
ped <- readRDS(build("Rds/ped_vars.RDS"))
nova_ped <- readRDS(build("Rds/nova_ped_vars.RDS"))

# Estações BRT ----
estacoes_brt <- readRDS(build("rds/geo_estacoes_brt.RDS"))

# Linhas BRT ---
linha_brt <- st_read(build("Shapes/BRT/Linha_BRT.shp"))
linha_brt <- linha_brt[!st_is_empty(linha_brt), ] # Remove geometrias vazias
linha_brt <- st_zm(linha_brt, drop = TRUE, what = "ZM") # Remove Eixo Z

## Buffer Estações BRT ----
buffer15 <- st_buffer(estacoes_brt, dist = 1500)
buffer20 <- st_buffer(estacoes_brt, dist = 2000)
buffer25 <- st_buffer(estacoes_brt, dist = 2500)
buffer30 <- st_buffer(estacoes_brt, dist = 3000)

# Ajusta geometria da malha de 2000
setores_validos <- st_make_valid(join_ped_09_16_malha_2000)
setores_validos <- st_make_valid(join_ped_16_19_malha_2000)

# Classifica Setores ----
sf_use_s2(FALSE)
## PED ----
grupos_15_ped_09_16 <- classifica_setor(join_ped_09_16_malha_2000, buffer15)
grupos_20_ped_09_16 <- classifica_setor(join_ped_09_16_malha_2000, buffer20)
grupos_25_ped_09_16 <- classifica_setor(join_ped_09_16_malha_2000, buffer25)
grupos_30_ped_09_16 <- classifica_setor(join_ped_09_16_malha_2000, buffer30)

## Nova PED ----
grupos_15_ped_16_19 <- classifica_setor(join_ped_16_19_malha_2000, buffer15) |> mutate(grupo = ifelse(CODSETOR2000 == 530010805250113,0,grupo))
grupos_20_ped_16_19 <- classifica_setor(join_ped_16_19_malha_2000, buffer20) |> mutate(grupo = ifelse(CODSETOR2000 == 530010805250113,0,grupo))
grupos_25_ped_16_19 <- classifica_setor(join_ped_16_19_malha_2000, buffer25) |> mutate(grupo = ifelse(CODSETOR2000 == 530010805250113,0,grupo))
grupos_30_ped_16_19 <- classifica_setor(join_ped_16_19_malha_2000, buffer30) |> mutate(grupo = ifelse(CODSETOR2000 == 530010805250113,0,grupo))
sf_use_s2(TRUE) 

# Cria grupos de Tratamento ----
# PED
join_09_16 <- ped |>
  mutate(conglom = as.double(substr(conglom,1,6))) |> 
  left_join(grupos_15_ped_09_16 |> rename(grupo_15 = grupo) |> st_drop_geometry()) |> 
  left_join(grupos_20_ped_09_16 |> rename(grupo_20 = grupo) |> st_drop_geometry()) |> 
  left_join(grupos_25_ped_09_16 |> rename(grupo_25 = grupo) |> st_drop_geometry()) |> 
  left_join(grupos_30_ped_09_16 |> rename(grupo_30 = grupo) |> st_drop_geometry()) |> 
  filter(!is.na(CODSETOR2000)) |> unique()

# Nova PED
join_16_19 <- nova_ped |> 
  left_join(grupos_15_ped_16_19 |> rename(grupo_15 = grupo) |> st_drop_geometry()) |> 
  left_join(grupos_20_ped_16_19 |> rename(grupo_20 = grupo) |> st_drop_geometry()) |> 
  left_join(grupos_25_ped_16_19 |> rename(grupo_25 = grupo) |> st_drop_geometry()) |> 
  left_join(grupos_30_ped_16_19 |> rename(grupo_30 = grupo) |> st_drop_geometry()) |> 
  filter(!is.na(CODSETOR2000)) |> unique()

# mapview(grupos_20_ped_16_19 |> filter(CODSETOR2000 != 530010805250113),zcol = "grupo",alpha.regions = .4) +
# mapview(grupos_20_ped_09_16 |> filter(CODSETOR2000 != 530010805250113),zcol = "grupo",alpha.regions = .4) +
# mapview(linha_brt,lwd = 5, color = "darkgrey") +
# mapview(buffer20, col.regions = "#fde333", alpha.regions = .1) + 
# mapview(estacoes_brt, col.regions = "#fde333", alpha.regions = 1) 

# Bind
base <- rbind(
  join_09_16,
  join_16_19
) |> 
  mutate(intervencao = ifelse(ano < 2014,0,1),
         ano = as.character(ano),
         rend_liquido = ifelse(rend_liquido == -0.01,NA_integer_,rend_liquido),
         ln_rend_bruto = ifelse(!is.na(rend_bruto),log(rend_bruto),0),
         ln_rend_liquido = ifelse((!is.na(rend_liquido) & rend_liquido != 0) ,log(rend_liquido),0),
         ln_horas_trab = ifelse(!is.na(horas_trab),log(horas_trab),0),
         ) 
  
# Cria Dummies
base <- dummy_cols(base)

# Salva
saveRDS(base, analysis("dados/base.RDS"))