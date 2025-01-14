source("config.R")

#'*Empilha listagens de endereços*

#  Os arquivos GP representam os arquivos de amostra utilizados na PED no processo
# de amostragem. Este script lê e empilha as bases de endereços. Em alguns meses
# os arquivos só estão disponíveis em .sav, e em outros só em .xlsx.


# Listando os arquivos GP ----
arquivos_gp_xls <- list.files(path = build("Dados/BasesGP"), 
                              pattern = "\\.xls$|\\.xlsx$", 
                              full.names = TRUE)

arquivos_gp_sav <- list.files(path = build("Dados/BasesGP"), 
                              pattern = "\\.sav$", 
                              full.names = TRUE)[c(1, 3, 13, 14, 39)]


# Lendo e empilhando arquivos GP nos formatos .xls e .xlsx
bind_GP_xls <- ler_empilhar_arquivos(arquivos_gp_xls, read_xls)

# Lendo e empilhando arquivos GP no formato .sav
bind_GP_sav <- ler_empilhar_arquivos(arquivos_gp_sav, read_sav) |> 
  mutate(ra= case_when(
    ra == 1 ~ "Plano Piloto",
    ra == 2 ~ "Gama",
    ra == 3 ~ "Taguatinga",
    ra == 4 ~ "Brazlândia",
    ra == 5 ~ "Sobradinho",
    ra == 6 ~ "Planaltina",
    ra == 7 ~ "Paranoá",
    ra == 8 ~ "Núcleo Bandeirante",
    ra == 9 ~ "Ceilândia",
    ra == 10 ~ "Guará",
    ra == 11 ~ "Cruzeiro",
    ra == 12 ~ "Samambaia",
    ra == 13 ~ "Santa Maria",
    ra == 14 ~ "São Sebastião",
    ra == 15 ~ "Recanto Das Emas",
    ra == 16 ~ "Lago Sul",
    ra == 17 ~ "Riacho Fundo",
    ra == 18 ~ "Lago Norte",
    ra == 19 ~ "Candangolândia"
  ))

# Combinando os dados dos arquivos GP, excluindo as colunas "ra" e "super"
bind_GP <- rbind(
  bind_GP_sav[, !names(bind_GP_sav) %in% c("super")],
  bind_GP_xls[, !names(bind_GP_xls) %in% c("super")]
) |> 
  mutate(endereco = str_squish(endereço),
         domic = etiqueta,
         ra = str_to_title(str_trim(ra))) |> 
  select(ano,mes,ra,domic)

#'*Cria base de relacção entre comicílio e Setor da Nova PED*
# Base de compatibilização de malhas
relacao_malhas <- read_sav(build("relacao/malhas2000e2010/Compatibiliza DF FINAL.sav")) |> 
  select(CODSETOR2010,reg = NOM_SD) |> 
  mutate(reg = str_to_title((str_trim(reg))))

## Relação domic setor censitário ----
arquivos <- list.files(
  path = build("relacao"), 
  pattern = "\\.sav$", 
  full.names = TRUE
)

# Lendo e empilhando os arquivos PED ----
## PED ----
relacao <- lapply(arquivos, ler_e_transformar) |> 
  bind_rows() |> 
  rename(aamm = ano_mes, domic = num_domicilio,CODSETOR2010 = codsetor2010) |> 
  left_join(relacao_malhas) |> 
  select(-CODSETOR2010)
  
#'*Lendo as bases da PED*

# Deflator
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

  # Microdados
ped <- readRDS(build("Rds/ped_vars.RDS"))
nova_ped <- readRDS(build("Rds/nova_ped_vars.RDS"))
dados_cs <- readRDS(build("dados/dados_controle_sintetico.RDS"))

ped <- ped |> 
  left_join(bind_GP) |> 
  mutate(
    class_conglom = case_when(
      substr(conglom,1,2) == 31 ~ "Ceilândia",
      substr(conglom,1,2) == 25 ~ "Taguatinga",
      substr(conglom,1,2) == 23 ~ "Plano Piloto",
      substr(conglom,1,2) == 35 ~ "Samambaia",
      substr(conglom,1,2) == 27 ~ "Sobradinho",
      substr(conglom,1,2) == 28 ~ "Planaltina",
      substr(conglom,1,2) == 33 ~ "Guará",
      substr(conglom,1,2) == 38 ~ "Recanto Das Emas",
      substr(conglom,1,2) == 24 ~ "Gama",
      substr(conglom,1,2) == 36 ~ "Santa Maria",
      substr(conglom,1,2) == 34 ~ "Cruzeiro",
      substr(conglom,1,2) == 40 ~ "Riacho Fundo",
      substr(conglom,1,2) == 37 ~ "São Sebastião",
      substr(conglom,1,2) == 39 ~ "Lago Sul",
      substr(conglom,1,2) == 26 ~ "Brazlândia",
      substr(conglom,1,2) == 41 ~ "Lago Norte",
      substr(conglom,1,2) == 30 ~ "Núcleo Bandeirante",
      substr(conglom,1,2) == 29 ~ "Paranoá",
      substr(conglom,1,2) == 42 ~ "Candangolândia")) |> 
  mutate(reg = ifelse(!is.na(ra),ra,class_conglom)) |> 
  select(-c(ra,class_conglom))

nova_ped <- nova_ped |> 
  filter(ano < 2020) |> 
  left_join(relacao)

# Criar um vetor de mapeamento
correspondencia <- c(
  "Brasilia" = "Plano Piloto",
  "Gama" = "Gama",
  "Taguatinga" = "Taguatinga",
  "Brazlandia" = "Brazlândia",
  "Sobradinho" = "Sobradinho",
  "Planaltina" = "Planaltina",
  "Paranoa" = "Paranoá",
  "Riacho Fundo" = "Riacho Fundo",
  "Nucleo Bandeirante" = "Núcleo Bandeirante",
  "Nbandeirante" = "Núcleo Bandeirante",
  "Ceilandia" = "Ceilândia",
  "Guara" = "Guará",
  "Cruzeiro" = "Cruzeiro",
  "Samambaia" = "Samambaia",
  "Candangolandia" = "Candangolândia",
  "Recanto Das Emas" = "Recanto Das Emas",
  "Lago Norte" = "Lago Norte",
  "Lago Sul" = "Lago Sul",
  "Santa Maria" = "Santa Maria",
  "Sao Sebastiao" = "São Sebastião"
)

nova_ped$reg <- correspondencia[nova_ped$reg]

# Bind
base <- rbind(
  ped,
  nova_ped
)

base <- base |> 
  mutate(aamm1=zoo::as.yearmon(as.character(aamm), "%Y%m")) |> 
  left_join(deflator) |> 
  mutate(intervencao = ifelse(ano < 2014,0,1),
         ano = as.double(ano),
         #horas_trab = ifelse(is.na(horas_trab),0,horas_trab),
         rend_liquido = ifelse(rend_liquido == -0.01,0,rend_liquido/deflator),
         rend_bruto = ifelse(rend_bruto == -0.01,NA,rend_bruto/deflator),
         ln_rend_bruto = ifelse(!is.na(rend_bruto),log(rend_bruto/deflator),NA),
         ln_rend_liquido = ifelse((!is.na(rend_liquido) & rend_liquido != 0) ,log(rend_liquido/deflator),0),
         ln_horas_trab = ifelse(horas_trab == 0,0,log(horas_trab)),
         en_sup = ifelse(escol == "sup_com",1,0),
         negro = ifelse(cor %in% c("preta","parda"),1,0),
         idade2 = sqrt(idade)
  )


saveRDS(base,analysis("dados/base_ra.RDS"))
write.csv(base,analysis("dados/base_ra.csv"))



