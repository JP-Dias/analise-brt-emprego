#'*Script que traz as informações setoriais da malha de 2010 para a malha de 2000*
#'*A malha de 2000 tem um problema de projeção, então o script também resolve isso*

# Setores PED 2010 ---
source(build("R/3 - cria bases correspondencia de malhas.R"))

# Base de compatibilização de malhas
relacao_malhas <- read_sav(build("relacao/malhas2000e2010/Compatibiliza DF FINAL.sav"))

# Malhas Censitárias ----
malha_2000 <- read_sf(build("Shapes/2000/5300108.SHP"))
malha_2010 <- read_sf(build("Shapes/2010/53SEE250GC_SIR.shp"))

malha_2000 <- st_set_crs(malha_2000, 32723)
malha_2000 <- st_transform(malha_2000, 32723)

# Corrije problema de projeção das malhas ----
# Altera padrão das malhas
malha_2000 <- st_transform(malha_2000, 4674)  # Reprojetando para SIRGAS 2000
malha_2010 <- st_transform(malha_2010, 4674)  # Reprojetando para SIRGAS 2000

# Calculando a diferença de deslocamento 
dx <- 0.0007  # Defina o valor de deslocamento no eixo x
dy <- -0.0003  # Defina o valor de deslocamento no eixo y

# Aplicando a translação nas coordenadas da malha
malha_2000_translated <- st_geometry(malha_2000) + c(dx, dy)

# Atualizando a geometria da malha
st_geometry(malha_2000) <- malha_2000_translated

# Reatribuindo o CRS original à malha
st_crs(malha_2000) <- st_crs(malha_2010)  


# Junta malha de 2000 com base de relação entre as malhas ----
malha_2000_join <- malha_2000 |> 
  rename(CODSETOR2000 = ID_) |> 
  mutate(CODSETOR2000 = as.numeric(CODSETOR2000)) |> 
  full_join(relacao_malhas,by = "CODSETOR2000") 

# Prepara setores ped para join ----
setores_ped_09_16 <- malha_ped_09_16 |> mutate(CODSETOR2010 = as.double(CD_GEOCODI)) 
setores_ped_16_19 <- malha_ped_16_19 |> mutate(CODSETOR2010 = as.double(CD_GEOCODI))

# Junta Malha e Setores PED ---- 

join_ped_09_16_malha_2000 <- malha_2000_join |> 
  full_join(setores_ped_09_16) |> 
  select(CODSETOR2000,CODSETOR2010,NM_SUBDIST,conglom, ano, mes, domic) |> 
  mutate(conglom = as.double(conglom)) |> 
  na.omit() |> unique() 
  
join_ped_16_19_malha_2000 <- malha_2000_join |> 
  full_join(setores_ped_16_19) |>
  select(CODSETOR2000,CODSETOR2010,NM_SUBDIST,ano, mes, domic) |> 
  filter(NM_SUBDIST %in% c("GAMA", "SANTA MARIA")) |> 
  na.omit() |> unique() 


# mapview(join_ped_09_16_malha_2000, col.regions = "blue" ,col = "blue",alpha.regions = 00.1) + mapview(join_ped_16_19_malha_2000, col.regions = "red" ,col = "red",alpha.regions = 00.1)
