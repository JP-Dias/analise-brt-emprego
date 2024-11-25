#'*Prepara Microdados PED, Nova PED e geo das Estações BRT*

source("config.R")

# PED ----
ped <- readRDS(build("Rds/ped_empilhada.RDS"))

## Seleção e renomeação das variáveis ----
dados_ped <- ped |> 
  mutate(
    fator = fator/12,
    fem = case_when(c010 == 1 ~ 0, c010 == 2 ~ 1,TRUE ~ NA_integer_),
    mora_mesma_ra = ifelse(c071 == 1, 1, 0),
    ocupado = case_when(
      sit == 4 ~ 1,
      sit %in% 1:3 ~ 0,
      TRUE ~ NA_integer_
    )) |> 
  mutate(
    trab_plano = ifelse(q270 == 5301, 1, 0),
    trab_plano = ifelse(is.na(ocupado),NA_integer_,trab_plano),  
    informal = ifelse(pos == 2 | (pos %in% 5:6 & q280 == 2), 1, 0),
    informal = ifelse(is.na(ocupado),NA_integer_,informal),  
    rend_bruto = ifelse(as.numeric(q421) %in% c(0, 1e7 + 1), NA_integer_, as.numeric(q421)),
    rend_liquido = ifelse(as.numeric(q422) %in% c(-1,0, 1e7 + 1), NA_integer_, as.numeric(q422)),
    horas_trab = ifelse(as.numeric(q431) %in% c(0,-1,1e3,1e3 + 1), NA_integer_, as.numeric(q431)),
    escol = case_when(
      inst %in% c(2,3) ~ "analf",
      inst == 4 ~ "fund_inc",
      inst == 5 ~ "fund_com",
      inst == 6 ~ "med_inc",
      inst == 7 ~ "med_com",
      inst == 8 ~ "sup_inc",
      inst %in% c(9,10) ~ "sup_com",
      TRUE ~ NA_character_
    ),
    setor_atv = case_when(
      (setor_cnae == 2000 | setor == 200) ~ "indust",  
      (setor_cnae == 3000 | setor == 300) ~ "construc",
      (setor_cnae == 4000 | setor == 400) ~ "comerc",
      (setor_cnae == 5000 | setor %in% c(500,511)) ~ "servic",
      (setor_cnae == 9999 | setor == 600) ~ "outros",
      TRUE ~ NA_character_
    ),
    cor = case_when(
      c050 == 1 ~ "branca",
      c050 == 2 ~ "preta",
      c050 == 3 ~ "parda",
      c050 == 4 ~ "amarela",
      c050 == 0 ~ "sd",
      TRUE ~ NA_character_
    ),
    posicao_fam = case_when(
      c040 == 1 ~ "chefe",
      c040 == 2 ~ "conjuge",
      c040 == 3 ~ "filho",
      TRUE ~ "outros"
    )
  ) |> 
  select(
    aamm,
    ano, mes, # Ano e Mês
    conglom = conglom, # Conglomerado
    domic = domic, # Domicílio
    ocupado, # Situação ocupacional
    rend_bruto, # Rendimento bruto
    rend_liquido, # Rendimento líquido
    informal, # Trabalhador Informal
    horas_trab, # Horas trabalhadas no trabalho principal
    trab_plano, # Dummy de trabalha na RA Plano Piloto
    setor_atv, # Setor de atividade CNAE
    escol, # Grau de instrução
    idade = c020, # Idade
    cor, # Cor
    fem, # Sexo
    mora_mesma_ra, # Mora na Messma RA 12 meses atrás
    pessoas = a090, # Total de moradores no domicílio
    posicao_fam, # Posição na família
    peso = fator
  )

saveRDS(dados_ped,build("Rds/ped_vars.RDS"))

# Nova PED ----

nova_ped <- readRDS(build("Rds/nova_ped_empilhada.RDS"))

## Seleção e renomeação das variáveis ----
dados_nova_ped <- nova_ped |> 
  mutate(
    fator = fator/12,
    fem = case_when(c030 == 1 ~ 0, c030 == 2 ~ 1,TRUE ~ NA_integer_),
    mora_mesma_ra = ifelse(((m041 == 1 | m05a >= 1) | (m010 == 1 | m02a >= 1)), 1, 0),  # Residia na mesma RA 12 meses atrás ou sempre morou na RA
    ocupado = case_when(
      sit == 4 ~ 1,  # Situação ocupacional (V2009)
      sit %in% 1:3 ~ 0,
      TRUE ~ NA_integer_
    )) |> 
  mutate(
    trab_plano = ifelse(f202 == 5301, 1, 0),  # Local de trabalho principal 
    trab_plano = ifelse(is.na(ocupado),NA_integer_,trab_plano), 
    informal = ifelse(pos == 2 | (pos %in% 5:6 & (ano %in% c(2016,2017) & f250 == 2 | ano %in% c(2018,2019) & f280 == 2)), 1, 0),  # Posição na ocupação 
    informal = ifelse(is.na(ocupado),NA_integer_,informal), 
    rend_bruto = case_when(ano %in% c(2016,2017) ~ifelse(as.numeric(f331) %in% c(-0.01,-1,0,1e9,1e9 + 1), NA_integer_, as.numeric(f331)),
                           TRUE~ifelse(as.numeric(f481) %in% c(-0.01,-1,0,1e9,1e9 + 1), NA_integer_, as.numeric(f481))
                           ),
    rend_liquido = ifelse(as.numeric(f482) %in% c(-0.01,-1, 0,1e9,1e9 + 1), NA_integer_, as.numeric(f482)),  # Remuneração líquida (F482)
    horas_trab = case_when(ano %in% c(2016,2017) ~ifelse(as.numeric(f340) %in% c(-1, 0,1e3,1e3 + 1), NA_integer_, as.numeric(f340)),
              TRUE ~ifelse(as.numeric(f492) %in% c(-1, 0,1e3,1e3 + 1), NA_integer_, as.numeric(f492))),
    escol = case_when(
      inst %in% c(2,3) ~ "analf",
      inst == 4 ~ "fund_inc",
      inst == 5 ~ "fund_com",
      inst == 6 ~ "med_inc",
      inst == 7 ~ "med_com",
      inst == 8 ~ "sup_inc",
      inst == 9 ~ "sup_com",
      TRUE ~ NA_character_
    ),
    setor_atv = case_when(
      (setor_cnae == 2000) ~ "indust",  
      (setor_cnae == 3000) ~ "construc",
      (setor_cnae == 4000) ~ "comerc",
      (setor_cnae == 5000) ~ "servic",
      (setor_cnae == 9999) ~ "outros",
      TRUE ~ NA_character_
    ),
    cor = case_when(
      c040 == 1 ~ "branca",  
      c040 == 2 ~ "preta",
      c040 == 3 ~ "parda",
      c040 == 4 ~ "amarela",
      c040 == 5 ~ "indígena",
      c040 == 6 ~ "ns",
      TRUE ~ NA_character_
    ),
    posicao_fam = case_when(
      c061 == 1 ~ "chefe",
      c061 %in% c(2,3) ~ "cônjuge",
      c061 == 4 ~ "filho",
      TRUE ~ "outros"
    )
  ) |> 
  select(
    aamm,
    ano, mes, # Ano e Mês
    conglom, # Conglomerado
    domic, # Domicílio
    ocupado, # Situação ocupacional
    rend_bruto, # Rendimento bruto
    rend_liquido, # Rendimento líquido
    informal, # Trabalhador Informal
    horas_trab, # Horas trabalhadas no trabalho principal
    trab_plano, # Dummy de trabalha na RA Plano Piloto
    setor_atv, # Setor de atividade CNAE
    escol, # Grau de instrução
    idade = c050, # Idade
    cor, # Cor
    fem, # Sexo
    mora_mesma_ra, # Mora na Messma RA 12 meses atrás
    pessoas = totmora, # Total de moradores no domicílio
    posicao_fam, # Posição na família
    peso = fator,
  )

saveRDS(dados_nova_ped,build("Rds/nova_ped_vars.RDS"))

table(nova_ped$ano,nova_ped$f492)


# Estações BRT ----
brt <- read_sf(build("Shapes/BRT/estacoes_BRT.shp")) |> slice(1:11)

estacoes_brt <- brt |> 
  mutate(lon_brt = st_coordinates(geometry)[,1],
         lat_brt = st_coordinates(geometry)[,2],) |> 
  select(ra = Name, lon_brt, lat_brt) |>
  st_drop_geometry() |> 
  filter(ra %in% c("Gama","Santa Maria")) 

estacoes_brt_sf <- estacoes_brt |> st_as_sf(coords = c("lon_brt", "lat_brt"), crs = 4326)

## Salva Objeto ----
saveRDS(estacoes_brt_sf,build("Rds/geo_estacoes_brt.RDS"))

