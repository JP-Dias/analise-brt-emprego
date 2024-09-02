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
  mutate(ra = case_when(
    ra == 1 ~ "Brasília",
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
    ra == 15 ~ "Recanto das Emas",
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
         domic = etiqueta) |> 
  select(-c(endereço,etiqueta))

# Salva base
saveRDS(bind_GP,build("Rds/base_enderecos_ped.RDS"))

