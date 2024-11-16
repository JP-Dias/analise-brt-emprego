source("config.R")

`%notin%` <- negate(`%in%`)


# Lê bases ----
## PED ----
ped <- readRDS(build("Rds/ped_empilhada.RDS"))
malha_2010 <- st_read(build("Shapes/2010/53SEE250GC_SIR.shp"), options = "ENCODING=WINDOWS-1252")

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

ped <- ped |> 
  mutate(aamm1=zoo::as.yearmon(as.character(aamm), "%Y%m")) |> 
  left_join(deflator)


#mapview(malha_2010,zcol = "NM_SUBDIST",color = "white")

malha_subdist <- malha_2010  |> 
  mutate(reg = ifelse(NM_SUBDIST == "BRASÍLIA","Plano Piloto",str_to_title(NM_SUBDIST))) |>
  st_make_valid() |> 
  group_by(reg) |> 
  summarise(geometry = st_union(geometry),
            area = st_area(geometry))

#mapview(malha_subdist)

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
  cong == 38 ~ "Recanto Das Emas",
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
  mutate(
    local_trab = case_when(
      grepl("^52|^30", q270) ~ "Entorno",
      q270 %in% c(5399, 9994) ~ "Outro",
      TRUE ~ case_when(
        q270 == 5301 ~ "Plano Piloto",
        q270 == 5302 ~ "Gama",
        q270 == 5303 ~ "Taguatinga",
        q270 == 5304 ~ "Brazlândia",
        q270 == 5305 ~ "Sobradinho",
        q270 == 5306 ~ "Planaltina",
        q270 == 5307 ~ "Paranoá",
        q270 == 5308 ~ "Núcleo Bandeirante",
        q270 == 5309 ~ "Ceilândia",
        q270 == 5310 ~ "Guará",
        q270 == 5311 ~ "Cruzeiro",
        q270 == 5312 ~ "Samambaia",
        q270 == 5313 ~ "Santa Maria",
        q270 == 5314 ~ "São Sebastião",
        q270 == 5315 ~ "Recanto Das Emas",
        q270 == 5316 ~ "Lago Sul",
        q270 == 5317 ~ "Riacho Fundo",
        q270 == 5318 ~ "Lago Norte",
        q270 == 5319 ~ "Candangolândia",
      )
    )
  ) |> 
  mutate(mora_trabalha = ifelse(local_trab == reg,1,0))
  
#prop.table(table(base$reg,base$mora_trabalha),1)*100

base <- base |> as_survey(weights = fator_ano)  
  

perc_analf <- base |> 
  filter(ano == 2009, idade > 18) |> 
  group_by(reg) |> 
  summarise(perc_analf = survey_mean(analf,na.rm = T, vartype = "cv")) |> 
  na.omit()

#left_join(malha_subdist, perc_analf) |> mapview(zcol = "perc_analf")


media_idade <- base |> 
  filter(ano == 2009) |> 
  group_by(reg) |> 
  summarise(media_idade = survey_mean(idade,na.rm = T, vartype = "cv")) |> 
  na.omit()

#left_join(malha_subdist, media_idade) |> mapview(zcol = "media_idade")


media_familia <- base |> 
  filter(ano == 2009) |> 
  group_by(reg) |> 
  summarise(media_familia = survey_mean(tamanho,na.rm = T, vartype = "cv")) |> 
  na.omit()

#left_join(malha_subdist, media_familia) |> mapview(zcol = "media_familia")

perc_idoso <- base |> 
  filter(ano == 2009) |> 
  group_by(reg) |> 
  summarise(perc_idoso = survey_mean(idoso,na.rm = T, vartype = "cv")) |> 
  na.omit()

#left_join(malha_subdist, perc_idoso) |> mapview(zcol = "perc_idoso")

perc_informal <- base |> 
  filter(ano == 2009) |> 
  group_by(reg) |> 
  summarise(perc_informal = survey_mean(informal,na.rm = T, vartype = "cv"))  |> 
  na.omit()

#left_join(malha_subdist, perc_informal) |> mapview(zcol = "perc_informal")

perc_trab_plano <- base |> 
  filter(ano == 2009) |> 
  group_by(reg) |> 
  summarise(perc_trab_plano = survey_mean(trab_plano,na.rm = T, vartype = "cv")) |> 
  na.omit()

#left_join(malha_subdist, perc_trab_plano) |> mapview(zcol = "perc_trab_plano")

perc_nasc_df <- base |> 
  filter(ano == 2009) |> 
  group_by(reg) |> 
  summarise(perc_nasc_df = survey_mean(nasc_df,na.rm = T, vartype = "cv")) |> 
  na.omit()

#left_join(malha_subdist, perc_nasc_df) |> mapview(zcol = "perc_nasc_df")

perc_mora_trabalha <- base |> 
  filter(ano == 2009) |> 
  group_by(reg) |> 
  summarise(perc_mora_trabalha = survey_mean(mora_trabalha,na.rm = T, vartype = "cv")) |> 
  na.omit()

#left_join(malha_subdist, perc_mora_trabalha) |> mapview(zcol = "perc_mora_trabalha")

n_empregos <- base |> 
  filter(ano == 2009,
         local_trab %notin% c("Outro","Entorno")) |> 
  mutate(reg = local_trab) |> 
  group_by(reg) |> 
  summarise(n_empregos = survey_total(na.rm = T, vartype = "cv")) |> 
  na.omit()

#left_join(malha_subdist, n_empregos) |> mapview(zcol = "n_empregos")

renda_fam_media <- base |> 
  filter(ano == 2009,
         rfam != -1000) |> 
  group_by(reg) |> 
  summarise(renda_fam_media = survey_mean(rfam,na.rm = T, vartype = "cv")) |> 
  na.omit()

#left_join(malha_subdist, n_empregos) |> mapview(zcol = "n_empregos")
