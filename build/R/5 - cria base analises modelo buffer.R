
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
buffer35 <- st_buffer(estacoes_brt, dist = 3500)
buffer45 <- st_buffer(estacoes_brt, dist = 4500)
buffer55 <- st_buffer(estacoes_brt, dist = 5500)
buffer65 <- st_buffer(estacoes_brt, dist = 6500)

# Ajusta geometria da malha de 2000
setores_validos <- st_make_valid(join_ped_09_16_malha_2000)
setores_validos <- st_make_valid(join_ped_16_19_malha_2000)

# Classifica Setores ----
sf_use_s2(FALSE)
## PED ----
grupos_15_ped_09_16 <- classifica_setor(join_ped_09_16_malha_2000, buffer15)
grupos_20_ped_09_16 <- classifica_setor(join_ped_09_16_malha_2000, buffer20)
grupos_35_ped_09_16 <- classifica_setor(join_ped_09_16_malha_2000, buffer35)
grupos_45_ped_09_16 <- classifica_setor(join_ped_09_16_malha_2000, buffer45)
grupos_55_ped_09_16 <- classifica_setor(join_ped_09_16_malha_2000, buffer55)
grupos_65_ped_09_16 <- classifica_setor(join_ped_09_16_malha_2000, buffer65)

## Nova PED ----
grupos_15_ped_16_19 <- classifica_setor(join_ped_16_19_malha_2000, buffer15) |> mutate(grupo = ifelse(CODSETOR2000 == 530010805250113,0,grupo))
grupos_20_ped_16_19 <- classifica_setor(join_ped_16_19_malha_2000, buffer20) |> mutate(grupo = ifelse(CODSETOR2000 == 530010805250113,0,grupo))
grupos_35_ped_16_19 <- classifica_setor(join_ped_16_19_malha_2000, buffer35) |> mutate(grupo = ifelse(CODSETOR2000 == 530010805250113,0,grupo))
grupos_45_ped_16_19 <- classifica_setor(join_ped_16_19_malha_2000, buffer45) |> mutate(grupo = ifelse(CODSETOR2000 == 530010805250113,0,grupo))
grupos_55_ped_16_19 <- classifica_setor(join_ped_16_19_malha_2000, buffer55) |> mutate(grupo = ifelse(CODSETOR2000 == 530010805250113,0,grupo))
grupos_65_ped_16_19 <- classifica_setor(join_ped_16_19_malha_2000, buffer65) |> mutate(grupo = ifelse(CODSETOR2000 == 530010805250113,0,grupo))
sf_use_s2(TRUE) 

# Cria grupos de Tratamento ----
# PED
join_09_16 <- ped |>
  mutate(conglom = as.double(substr(conglom,1,6))) |> 
  left_join(grupos_15_ped_09_16 |> rename(grupo_15 = grupo) |> st_drop_geometry()) |> 
  left_join(grupos_20_ped_09_16 |> rename(grupo_20 = grupo) |> st_drop_geometry()) |> 
  left_join(grupos_35_ped_09_16 |> rename(grupo_35 = grupo) |> st_drop_geometry()) |> 
  left_join(grupos_45_ped_09_16 |> rename(grupo_45 = grupo) |> st_drop_geometry()) |> 
  left_join(grupos_55_ped_09_16 |> rename(grupo_55 = grupo) |> st_drop_geometry()) |> 
  left_join(grupos_65_ped_09_16 |> rename(grupo_65 = grupo) |> st_drop_geometry()) |> 
  filter(!is.na(CODSETOR2000)) |> unique()

# Nova PED
join_16_19 <- nova_ped |> 
  left_join(grupos_15_ped_16_19 |> rename(grupo_15 = grupo) |> st_drop_geometry()) |> 
  left_join(grupos_20_ped_16_19 |> rename(grupo_20 = grupo) |> st_drop_geometry()) |> 
  left_join(grupos_35_ped_16_19 |> rename(grupo_35 = grupo) |> st_drop_geometry()) |> 
  left_join(grupos_45_ped_16_19 |> rename(grupo_45 = grupo) |> st_drop_geometry()) |> 
  left_join(grupos_55_ped_16_19 |> rename(grupo_55 = grupo) |> st_drop_geometry()) |> 
  left_join(grupos_65_ped_16_19 |> rename(grupo_65 = grupo) |> st_drop_geometry()) |> 
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
)

deflator <- rbind(
  get_sidra(api = "/t/2951/n6/5300108/v/44/p/all/c315/7169/d/v44%202"), # De 2006 até 2011
  get_sidra(api = "/t/1100/n6/5300108/v/44/p/all/c315/7169/d/v44%202"), # De 2012 até 2019
  get_sidra(api = "/t/7063/n6/5300108/v/44/p/all/c315/7169/d/v44%202")) |> # De 2020 em diante
  dplyr::select(aamm = "Mês (Código)", inpc = "Valor") |> 
  mutate(aamm1=zoo::as.yearmon(as.character(aamm), "%Y%m"),
         inpc = inpc / 100)|>
  group_by(aamm1)|>  #faz com que cada operação seja feita dentro de cada 'grupo' 
  summarise(inpc_mensal = prod(1 + inpc)) |>
  mutate(inpc_acumulado = cumprod(inpc_mensal),
         deflator = inpc_acumulado / last(inpc_acumulado)) |>   # Perído base: Jan/2023 
  dplyr::select(aamm1,deflator)

base <- base |> 
  mutate(aamm1=zoo::as.yearmon(as.character(aamm), "%Y%m")) |> 
  left_join(deflator) |> 
  mutate(intervencao = ifelse(ano < 2014,0,1),
         ano = as.character(ano),
         #horas_trab = ifelse(is.na(horas_trab),0,horas_trab),
         rend_liquido = ifelse(rend_liquido == -0.01,0,rend_liquido/deflator),
         rend_bruto = ifelse(rend_bruto == -0.01,NA,rend_bruto/deflator),
         ln_rend_bruto = ifelse(!is.na(rend_bruto),log(rend_bruto/deflator),NA),
         ln_rend_liquido = ifelse((!is.na(rend_liquido) & rend_liquido != 0) ,log(rend_liquido/deflator),0),
         ln_horas_trab = ifelse(horas_trab == 0,0,log(horas_trab)),
         en_sup = ifelse(escol == "sup_com",1,0),
         negro = ifelse(cor %in% c("preta","parda"),1,0),
         idade2 = sqrt(idade),
         setor = CODSETOR2000
         )# |>
  # select(aamm,ano,mes,bairro,setor,
  #        grupo_15,grupo_20,grupo_25,grupo_30,
  #        intervencao,ocupado,informal,trab_plano,
  #        horas_trab,ln_horas_trab,rend_bruto,ln_rend_bruto,
  #        en_sup,idade,idade2,fem,negro,mora_mesma_ra,pessoas)
  # 
# Cria Dummies
#base <- dummy_cols(base)

# Salva
saveRDS(base, analysis("dados/base.RDS"))

sum(base$grupo_20)
sum(base$grupo_35)
sum(base$grupo_45)
sum(base$grupo_55)
sum(base$grupo_65)
