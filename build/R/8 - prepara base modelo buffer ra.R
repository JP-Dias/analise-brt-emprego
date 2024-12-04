source("config.R")

base_buffer <- readRDS(analysis("dados/base.RDS")) 

base_ra <- readRDS(analysis("dados/base_ra.RDS")) |> 
  filter(reg %in% c("Recanto Das Emas","Sobradinho")) |> 
  mutate(grupo = 0,
         ano = as.character(ano),
         modelo = ifelse(reg == "Sobradinho","Gama","Santa Maria"))

base_buffer_gama <- base_buffer |> 
  filter(bairro %in% c("Setor Norte","Setor Oeste","Setor Sul","Setor Leste",
                       "Setor Central"),
         grupo_20 == 1) |> 
  mutate(grupo = 1,
         modelo = "Gama",
         reg = "Gama")

base_buffer_sm <- base_buffer |> 
  filter(bairro %in% c("Santa Maria Sul","Santa Maria Centro","Santos Dummont",
                       "Santa Maria Norte"),
         grupo_20 == 1) |> 
  mutate(grupo = 1,
         modelo = "Santa Maria",
         reg = "Santa Maria")
  
  
nomes_comuns <- intersect(names(base_buffer_gama), names(base_ra))

# Selecionar apenas os nomes em comum e empilhar as bases
base_empilhada <- bind_rows(
  base_buffer_gama %>% select(all_of(nomes_comuns)),
  base_buffer_sm %>% select(all_of(nomes_comuns)),
  base_ra %>% select(all_of(nomes_comuns))
)

saveRDS(base_empilhada,analysis("dados/base_buffer_ra.RDS"))
