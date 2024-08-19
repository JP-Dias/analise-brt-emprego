#'*Empilha microdados Nova PED 2016 a 2019*

# Pacotes ----
library(haven)
library(tidyverse)
library(data.table)
library(foreign)
library(readxl)
library(dplyr)
library(labelled)
library(survey)
library(srvyr)

# Listando arquivos .sav no diretório
arquivos_ped_sav <- list.files(path = "Dados/NovaPED", pattern = "\\.sav$", full.names = TRUE)

# Função para ler arquivos
ler_e_transformar <- function(arquivo) {
  print(arquivo)
  temp <- read_sav(arquivo)
  names(temp) <- str_to_lower(names(temp))
  temp
}

# Lendo e empilhando os arquivos PED
bind_nova_ped <- lapply(arquivos_ped_sav, ler_e_transformar) |> 
  bind_rows() |> 
  remove_var_label() |>  # Removendo rótulos das variáveis
  mutate(
    ano = as.numeric(substr(aamm, 1, 4)),
    mes = as.numeric(substr(aamm, 5, 6))
  )

# Salva base
saveRDS(bind_nova_ped,"Rds/nova_ped_empilhada.RDS")
