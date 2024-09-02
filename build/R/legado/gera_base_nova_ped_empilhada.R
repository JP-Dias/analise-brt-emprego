#'*Empilha microdados PED 2009 a 2016*
#'*Empilha microdados Nova PED 2016 a 2019*

source("config.R")

# Listando arquivos .sav no diretório

arquivos_ped_sav <- list.files(
  path = build("Dados/PED"), 
  pattern = "\\.sav$", 
  full.names = TRUE
  )

arquivos_nova_ped_sav <- list.files(
  path = build("Dados/NovaPED"), 
  pattern = "\\.sav$", 
  full.names = TRUE
  )


# Lendo e empilhando os arquivos PED

bind_ped <- lapply(arquivos_ped_sav, ler_e_transformar) |> 
  bind_rows() |> 
  remove_var_label() |>  # Removendo labels das variáveis
  mutate(
    ano = as.numeric(substr(aamm, 1, 4)),
    mes = as.numeric(substr(aamm, 5, 6))
  )

bind_nova_ped <- lapply(arquivos_nova_ped_sav, ler_e_transformar) |> 
  bind_rows() |> 
  remove_var_label() |>  # Removendo rótulos das variáveis
  mutate(
    ano = as.numeric(substr(aamm, 1, 4)),
    mes = as.numeric(substr(aamm, 5, 6))
  )

# Salva bases

saveRDS(bind_ped,build("Rds/ped_empilhada.RDS"))
saveRDS(bind_nova_ped,"Rds/nova_ped_empilhada.RDS")
