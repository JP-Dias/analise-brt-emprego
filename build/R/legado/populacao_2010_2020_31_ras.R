dir()

library(readxl)
projecao <- "Projeções-Populacionais-Estruturas-Etárias-por-RA-2010-2020.xlsx"

abas <- excel_sheets(projecao)

lista <- list()

for (i in 1:length(abas)) {
  
  lista[[i]] <- read_excel(projecao, sheet = abas[i], skip = 3)[36,seq(from = 2, to = 32, by = 3)] |> 
    gather("ano","pop") |> 
    mutate(reg = abas[i],
           ano = as.double(ano),
           pop = as.numeric(pop))
}

base_pop <- bind_rows(lista)

# base_pop <- base_pop |> 
#   mutate(reg = case_when(
#     reg %in% c("Vicente Pires","Águas Claras") ~ "Taguatinga"
#     reg %in% c("Sudoeste_Octogonal") ~ "Cruzeiro",
#     reg %in% c("SIA","SCIA","Guará",) ~ "Guará",
#     reg %in% c("Park Way","Núcleo Bandeirante") ~ "Núcleo Bandeirante",
#     reg %in% c("Park Way","Núcleo Bandeirante") ~ "Núcleo Bandeirante",
#   ))