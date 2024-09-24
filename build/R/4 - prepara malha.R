#'*Script que traz as informações setoriais da malha de 2010 para a malha de 2000*
#'*A malha de 2000 tem um problema de projeção, então o script também resolve isso*

# Setores PED 2010 ---
source("config.R")
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


bairros <- read_sf(build("Shapes/setor/setor.shp")) |> 
  filter(gdb_archiv %in% c(22, 95, 117, 129, 135, 152,
                           176, 185, 187,191, 194, 200,
                           205,260, 283, 290))

mapview(bairros)

setor0916 <- join_ped_09_16_malha_2000 |> select(CODSETOR2000,geometry) |> unique()
setor1619 <- join_ped_16_19_malha_2000 |> select(CODSETOR2000,geometry) |> unique()

setor0916 <- setor0916 |> 
  mutate(bairro = case_when(
    CODSETOR2000 == 530010805070062 ~ "Setor Norte",
    CODSETOR2000 == 530010805070052 ~ "Setor Oeste",
    CODSETOR2000 == 530010805070043 ~ "Setor Oeste",
    CODSETOR2000 == 530010805070032 ~ "Setor Oeste",
    CODSETOR2000 == 530010805070035 ~ "Setor Oeste",
    CODSETOR2000 == 530010805070036 ~ "Setor Oeste",
    CODSETOR2000 == 530010805070027 ~ "Setor Oeste",
    #
    CODSETOR2000 == 530010805070111 ~ "Setor Sul",
    CODSETOR2000 == 530010805070112 ~ "Setor Sul",
    CODSETOR2000 == 530010805070118 ~ "Setor Sul",
    CODSETOR2000 == 530010805070121 ~ "Setor Sul",
    CODSETOR2000 == 530010805070123 ~ "Setor Sul",
    CODSETOR2000 == 530010805070124 ~ "Setor Sul",
    CODSETOR2000 == 530010805070126 ~ "Setor Sul",
    CODSETOR2000 == 530010805070132 ~ "Setor Sul",
    CODSETOR2000 == 530010805070133 ~ "Setor Sul",
    CODSETOR2000 == 530010805070137 ~ "Setor Sul",
    CODSETOR2000 == 530010805070139 ~ "Setor Sul",
    #
    CODSETOR2000 == 530010805070069 ~ "Setor Leste",
    CODSETOR2000 == 530010805070074 ~ "Setor Leste",
    CODSETOR2000 == 530010805070070 ~ "Setor Leste",
    CODSETOR2000 == 530010805070072 ~ "Setor Leste",
    CODSETOR2000 == 530010805070076 ~ "Setor Leste",
    CODSETOR2000 == 530010805070077 ~ "Setor Leste",
    CODSETOR2000 == 530010805070078 ~ "Setor Leste",
    CODSETOR2000 == 530010805070084 ~ "Setor Leste",
    CODSETOR2000 == 530010805070082 ~ "Setor Leste",
    CODSETOR2000 == 530010805070081 ~ "Setor Leste",
    CODSETOR2000 == 530010805070086 ~ "Setor Leste",
    CODSETOR2000 == 530010805070088 ~ "Setor Leste",
    CODSETOR2000 == 530010805070090 ~ "Setor Leste",
    CODSETOR2000 == 530010805070091 ~ "Setor Leste",
    CODSETOR2000 == 530010805070092 ~ "Setor Leste",
    CODSETOR2000 == 530010805070093 ~ "Setor Leste",
    CODSETOR2000 == 530010805070094 ~ "Setor Leste",
    CODSETOR2000 == 530010805070095 ~ "Setor Leste",
    CODSETOR2000 == 530010805070097 ~ "Setor Leste",
    CODSETOR2000 == 530010805070099 ~ "Setor Leste",
    CODSETOR2000 == 530010805070099 ~ "Setor Leste",
    CODSETOR2000 == 530010805070108 ~ "Setor Leste",
    CODSETOR2000 == 530010805070109 ~ "Setor Leste",
    TRUE ~ "Não acabei"
  ))

mapview(setor0916,zcol = "bairro", alpha.regions = 0.5)

mapview(bairros) + 
  mapview(setor0916, z.col = , alpha.regions = 0.5)


