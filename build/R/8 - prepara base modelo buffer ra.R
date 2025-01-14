source("config.R")

base_buffer <- readRDS(analysis("dados/base.RDS")) 

base_ra <- readRDS(analysis("dados/base_ra.RDS")) |> 
  filter(reg %in% c("Recanto Das Emas","Brazlândia")) |> 
  mutate(grupo = 0,
         ano = as.character(ano),
         modelo = ifelse(reg == "Brazlândia","Gama","Santa Maria"))

#===============================================================================

base_buffer_gama <- base_buffer |> 
  filter(bairro %in% c("Setor Norte","Setor Oeste","Setor Sul","Setor Leste",
                       "Setor Central"),
         grupo_15 == 1) |> 
  mutate(grupo = 1,
         modelo = "Gama",
         reg = "Gama")

base_buffer_sm <- base_buffer |> 
  filter(bairro %in% c("Santa Maria Sul","Santa Maria Centro","Santos Dummont",
                       "Santa Maria Norte"),
         grupo_15 == 1) |> 
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

saveRDS(base_empilhada,analysis("dados/base_buffer_ra_15.RDS"))

#===============================================================================

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

saveRDS(base_empilhada,analysis("dados/base_buffer_ra_20.RDS"))

#===============================================================================

base_buffer_gama <- base_buffer |> 
  filter(bairro %in% c("Setor Norte","Setor Oeste","Setor Sul","Setor Leste",
                       "Setor Central"),
         grupo_35 == 1) |> 
  mutate(grupo = 1,
         modelo = "Gama",
         reg = "Gama")

base_buffer_sm <- base_buffer |> 
  filter(bairro %in% c("Santa Maria Sul","Santa Maria Centro","Santos Dummont",
                       "Santa Maria Norte"),
         grupo_35 == 1) |> 
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

saveRDS(base_empilhada,analysis("dados/base_buffer_ra_35.RDS"))

#===============================================================================

base_buffer_gama <- base_buffer |> 
  filter(bairro %in% c("Setor Norte","Setor Oeste","Setor Sul","Setor Leste",
                       "Setor Central"),
         grupo_45 == 1) |> 
  mutate(grupo = 1,
         modelo = "Gama",
         reg = "Gama")

base_buffer_sm <- base_buffer |> 
  filter(bairro %in% c("Santa Maria Sul","Santa Maria Centro","Santos Dummont",
                       "Santa Maria Norte"),
         grupo_45 == 1) |> 
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

saveRDS(base_empilhada,analysis("dados/base_buffer_ra_45.RDS"))

#===============================================================================

base_buffer_gama <- base_buffer |> 
  filter(bairro %in% c("Setor Norte","Setor Oeste","Setor Sul","Setor Leste",
                       "Setor Central"),
         grupo_55 == 1) |> 
  mutate(grupo = 1,
         modelo = "Gama",
         reg = "Gama")

base_buffer_sm <- base_buffer |> 
  filter(bairro %in% c("Santa Maria Sul","Santa Maria Centro","Santos Dummont",
                       "Santa Maria Norte"),
         grupo_55 == 1) |> 
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

saveRDS(base_empilhada,analysis("dados/base_buffer_ra_55.RDS"))

#===============================================================================

base_buffer_gama <- base_buffer |> 
  filter(bairro %in% c("Setor Norte","Setor Oeste","Setor Sul","Setor Leste",
                       "Setor Central"),
         grupo_65 == 1) |> 
  mutate(grupo = 1,
         modelo = "Gama",
         reg = "Gama")

base_buffer_sm <- base_buffer |> 
  filter(bairro %in% c("Santa Maria Sul","Santa Maria Centro","Santos Dummont",
                       "Santa Maria Norte"),
         grupo_65 == 1) |> 
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

saveRDS(base_empilhada,analysis("dados/base_buffer_ra_65.RDS"))