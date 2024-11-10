source("config.R")

# PED ----
ped <- readRDS(build("Rds/ped_empilhada.RDS"))

## Seleção e renomeação das variáveis ----

base <- ped |> 
  filter(ano < 2014) |> 
  mutate(cong = substr(conglom,1,2)) |> 
  mutate(
  reg = case_when(
  cong == 31 ~ "Ceilândia",
  cong == 25 ~ "Taguatinga",
  cong == 23 ~ "Plano Piloto",
  cong == 35 ~ "Samambaia",
  cong == 27 ~ "Sobradinho",
  cong == 28 ~ "Planaltina",
  cong == 33 ~ "Guará",
  cong == 38 ~ "Recanto das Ema",
  cong == 24 ~ "Gama",
  cong == 36 ~ "Santa Maria",
  cong == 34 ~ "Cruzeiro",
  cong == 40 ~ "Riacho Fundo",
  cong == 37 ~ "São Sebastião",
  cong == 39 ~ "Lago Sul",
  cong == 26 ~ "Brazlândia",
  cong == 41 ~ "Lago Norte",
  cong == 30 ~ "Núcleo Bandeirante",
  cong == 29 ~ "Paranoá",
  cong == 42 ~ "Candangolândia"
  ),
  fator_ano  = fator/12,
  fator_ano3 = fator3/12,
  analf = ifelse(inst == 2,1,0),
  idade = c020,
  idoso = ifelse(idade>= 60 ,1,0),
  nasc_df = ifelse(c060 != 53 ,0,1),
  fem = case_when(c010 == 1 ~ 0, c010 == 2 ~ 1,TRUE ~ NA_integer_),
  mora_mesma_ra = ifelse(c071 == 1, 1, 0),
  ocupado = case_when(
    sit == 4 ~ 1,
    sit %in% 1:3 ~ 0,
    TRUE ~ NA_integer_
  ),
  trab_plano = ifelse(q270 == 5301, 1, 0),
  trab_plano = ifelse(is.na(ocupado),NA_integer_,trab_plano),  
  informal = ifelse(pos == 2 | (pos %in% 5:6 & q280 == 2), 1, 0),
  informal = ifelse(is.na(ocupado),NA_integer_,informal),  
  rend_bruto = ifelse(as.numeric(q421) %in% c(0, 1e7 + 1), NA_integer_, as.numeric(q421)),
  rend_liquido = ifelse(as.numeric(q422) %in% c(-1,0, 1e7 + 1), NA_integer_, as.numeric(q422)),
  horas_trab = ifelse(as.numeric(q431) %in% c(0,-1,1e3,1e3 + 1), NA_integer_, as.numeric(q431))
  ) |> 
  as_survey(weights = fator_ano)

cbind(
base |> 
  filter(ano == 2009, idade > 18) |> 
  group_by(reg) |> 
  summarise(perc_analf = survey_mean(analf,na.rm = T, vartype = "cv")),
base |> 
  filter(ano == 2009) |> 
  group_by(reg) |> 
  summarise(media_idade = survey_mean(idade,na.rm = T, vartype = "cv")),

base |> 
  filter(ano == 2009) |> 
  group_by(reg) |> 
  summarise(media_familia = survey_mean(tamanho,na.rm = T, vartype = "cv")),

base |> 
  filter(ano == 2009) |> 
  group_by(reg) |> 
  summarise(perc_idoso = survey_mean(idoso,na.rm = T, vartype = "cv")),


)

base |> 
  filter(ano == 2009) |> 
  group_by(reg) |> 
  summarise(perc_informal = survey_mean(informal,na.rm = T, vartype = "cv"))

base |> 
  filter(ano == 2009) |> 
  group_by(reg) |> 
  summarise(perc_trab_plano = survey_mean(trab_plano,na.rm = T, vartype = "cv"))

base |> 
  filter(ano == 2009) |> 
  group_by(reg) |> 
  summarise(perc_nasc_df = survey_mean(nasc_df,na.rm = T, vartype = "cv"))


