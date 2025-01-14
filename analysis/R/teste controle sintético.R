#install.packages("Synth")
library(Synth)
library(haven)

# Leitura da base de dados
base <- readRDS("analysis/dados/base_ra.RDS") 

# Criação da Variável de Efeito ----

## Gama ----
dados <- base |> 
  filter(!is.na(ocupado), idade %in% c(18:64), mora_mesma_ra == 1) |>  # Filtra ocupados, entre 18 e 64 anos e que moram na mesma RA nos últimos 12 meses 
  mutate(painel = case_when(mes %in% c(1,4,7,10)~"A",
                            mes %in% c(2,5,8,11)~"B",
                            TRUE~"C")) # Cria uma variável que sinaliza o Painel (A PED é feita em 3 paineis rotativos)


dados <- dados |> 
  group_by(reg,ano) |> 
  summarise(
    en_sup = mean(en_sup,na.rm = T),
    fem = mean(fem),
    negro = mean(negro),
    pessoas = mean(pessoas),
    rend_bruto = mean(rend_bruto, na.rm = T),
    ocupado = mean(ocupado,na.rm = T),
    informal = mean(informal),
    horas = mean(horas_trab,na.rm = T)
            ) |> 
  ungroup() |> 
  mutate(reg_id = as.numeric(as.factor(reg))) |> 
  as.data.frame() |> na.omit()

str(dados)


dados_synth <- dataprep(
  foo = dados,                     # DataFrame com os dados
  predictors = c("en_sup", "fem", "negro", "pessoas"), # Variáveis preditoras
  predictors.op = "mean",               # Média das variáveis preditoras
  dependent = "rend_bruto",             # Variável dependente
  unit.variable = "reg_id",            # Nome da coluna identificadora das RAs
  time.variable = "ano",                # Nome da coluna com o tempo
  treatment.identifier = "Gama",        # RA tratada (por exemplo, Gama)
  controls.identifier = c("Taguatinga","Ceilândia","Brazlândia","Sobradinho",
                          "Planaltina","Paranoá","Núcleo Bandeirante","Guará",
                          "Cruzeiro","Samambaia","São Sebastião","Recanto Das Emas",
                          "Lago Sul","Riacho Fundo","Lago Norte","Candangolândia"), # RAs de controle
  time.predictors.prior = 2009:2013,    # Período pré-tratamento
  time.optimize.ssr = 2009:2013,        # Período usado para otimizar os pesos
  unit.names.variable = "reg",      # Nome das unidades
  time.plot = 2010:2017                 # Período total de análise
)


# Executando o controle sintético
modelo_synth <- synth(data.prep.obj = dados_synth)

# Resultados
synth.tables <- synth.tab(dataprep.res = dados_synth, synth.res = modelo_synth)
print(synth.tables)


path.plot(synth.res = modelo_synth, dataprep.res = dados_synth, 
          Ylab = "Rendimentos Brutos", 
          Xlab = "Ano", 
          Legend = c("Gama", "Sintético"))


dados_synth <- dataprep(
  foo = dados,                     # DataFrame com os dados
  predictors = c("en_sup", "fem", "negro", "pessoas"), # Variáveis preditoras
  predictors.op = "mean",               # Média das variáveis preditoras
  dependent = "ocupado",             # Variável dependente
  unit.variable = "reg_id",            # Nome da coluna identificadora das RAs
  time.variable = "ano",                # Nome da coluna com o tempo
  treatment.identifier = "Gama",        # RA tratada (por exemplo, Gama)
  controls.identifier = c("Recanto Das Emas", "Planaltina", "Taguatinga",
                          "Brazlândia","Guará","Samambaia","Sobradinho",
                          "Núcleo Bandeirante","Candangolândia","Ceilândia"), # RAs de controle
  time.predictors.prior = 2009:2013,    # Período pré-tratamento
  time.optimize.ssr = 2009:2013,        # Período usado para otimizar os pesos
  unit.names.variable = "reg",      # Nome das unidades
  time.plot = 2009:2019                 # Período total de análise
)

# Executando o controle sintético
modelo_synth <- synth(data.prep.obj = dados_synth)

# Resultados
synth.tables <- synth.tab(dataprep.res = dados_synth, synth.res = modelo_synth)
print(synth.tables)


path.plot(synth.res = modelo_synth, dataprep.res = dados_synth, 
          Ylab = "Ocupação", 
          Xlab = "Ano", 
          Legend = c("Gama", "Sintético"))


dados_synth <- dataprep(
  foo = dados,                     # DataFrame com os dados
  predictors = c("en_sup", "fem", "negro", "pessoas"), # Variáveis preditoras
  predictors.op = "mean",               # Média das variáveis preditoras
  dependent = "informal",             # Variável dependente
  unit.variable = "reg_id",            # Nome da coluna identificadora das RAs
  time.variable = "ano",                # Nome da coluna com o tempo
  treatment.identifier = "Gama",        # RA tratada (por exemplo, Gama)
  controls.identifier = c("Recanto Das Emas", "Planaltina", "Taguatinga",
                          "Brazlândia","Guará","Samambaia","Sobradinho",
                          "Núcleo Bandeirante","Candangolândia","Ceilândia"), # RAs de controle
  time.predictors.prior = 2009:2013,    # Período pré-tratamento
  time.optimize.ssr = 2009:2013,        # Período usado para otimizar os pesos
  unit.names.variable = "reg",      # Nome das unidades
  time.plot = 2009:2019                 # Período total de análise
)

# Executando o controle sintético
modelo_synth <- synth(data.prep.obj = dados_synth)

# Resultados
synth.tables <- synth.tab(dataprep.res = dados_synth, synth.res = modelo_synth)
print(synth.tables)


path.plot(synth.res = modelo_synth, dataprep.res = dados_synth, 
          Ylab = "Informal", 
          Xlab = "Ano", 
          Legend = c("Gama", "Sintético"))


dados_synth <- dataprep(
  foo = dados,                     # DataFrame com os dados
  predictors = c("en_sup", "fem", "negro", "pessoas"), # Variáveis preditoras
  predictors.op = "mean",               # Média das variáveis preditoras
  dependent = "horas",             # Variável dependente
  unit.variable = "reg_id",            # Nome da coluna identificadora das RAs
  time.variable = "ano",                # Nome da coluna com o tempo
  treatment.identifier = "Gama",        # RA tratada (por exemplo, Gama)
  controls.identifier = c("Recanto Das Emas", "Planaltina", "Taguatinga",
                          "Brazlândia","Guará","Samambaia","Sobradinho",
                          "Núcleo Bandeirante","Candangolândia","Ceilândia"), # RAs de controle
  time.predictors.prior = 2009:2013,    # Período pré-tratamento
  time.optimize.ssr = 2009:2013,        # Período usado para otimizar os pesos
  unit.names.variable = "reg",      # Nome das unidades
  time.plot = 2009:2019                 # Período total de análise
)

# Executando o controle sintético
modelo_synth <- synth(data.prep.obj = dados_synth)

# Resultados
synth.tables <- synth.tab(dataprep.res = dados_synth, synth.res = modelo_synth)
print(synth.tables)


path.plot(synth.res = modelo_synth, dataprep.res = dados_synth, 
          Ylab = "Horas Trabalhadas", 
          Xlab = "Ano", 
          Legend = c("Gama", "Sintético"))



# ---------------------------------


dados_synth <- dataprep(
  foo = dados,                     # DataFrame com os dados
  predictors = c("en_sup", "fem", "negro", "pessoas"), # Variáveis preditoras
  predictors.op = "mean",               # Média das variáveis preditoras
  dependent = "rend_bruto",             # Variável dependente
  unit.variable = "reg_id",            # Nome da coluna identificadora das RAs
  time.variable = "ano",                # Nome da coluna com o tempo
  treatment.identifier = "Santa Maria",        # RA tratada (por exemplo, Santa Maria)
  controls.identifier = c("Taguatinga","Ceilândia","Brazlândia","Sobradinho",
                          "Planaltina","Paranoá","Núcleo Bandeirante","Guará",
                          "Cruzeiro","Samambaia","São Sebastião","Recanto Das Emas",
                          "Lago Sul","Riacho Fundo","Lago Norte","Candangolândia"), # RAs de controle
  time.predictors.prior = 2009:2013,    # Período pré-tratamento
  time.optimize.ssr = 2009:2013,        # Período usado para otimizar os pesos
  unit.names.variable = "reg",      # Nome das unidades
  time.plot = 2010:2017                 # Período total de análise
)

# Executando o controle sintético
modelo_synth <- synth(data.prep.obj = dados_synth)

# Resultados
synth.tables <- synth.tab(dataprep.res = dados_synth, synth.res = modelo_synth)
print(synth.tables)


path.plot(synth.res = modelo_synth, dataprep.res = dados_synth, 
          Ylab = "Rendimentos Brutos", 
          Xlab = "Ano", 
          Legend = c("Santa Maria", "Sintético"))


dados_synth <- dataprep(
  foo = dados,                     # DataFrame com os dados
  predictors = c("en_sup", "fem", "negro", "pessoas"), # Variáveis preditoras
  predictors.op = "mean",               # Média das variáveis preditoras
  dependent = "ocupado",             # Variável dependente
  unit.variable = "reg_id",            # Nome da coluna identificadora das RAs
  time.variable = "ano",                # Nome da coluna com o tempo
  treatment.identifier = "Santa Maria",        # RA tratada (por exemplo, Santa Maria)
  controls.identifier = c("Taguatinga","Ceilândia","Brazlândia","Sobradinho",
                          "Planaltina","Paranoá","Núcleo Bandeirante","Guará",
                          "Cruzeiro","Samambaia","São Sebastião","Recanto Das Emas",
                          "Lago Sul","Riacho Fundo","Lago Norte","Candangolândia"), # RAs de controle
  time.predictors.prior = 2009:2013,    # Período pré-tratamento
  time.optimize.ssr = 2009:2013,        # Período usado para otimizar os pesos
  unit.names.variable = "reg",      # Nome das unidades
  time.plot = 2009:2019                 # Período total de análise
)

# Executando o controle sintético
modelo_synth <- synth(data.prep.obj = dados_synth)

# Resultados
synth.tables <- synth.tab(dataprep.res = dados_synth, synth.res = modelo_synth)
print(synth.tables)


path.plot(synth.res = modelo_synth, dataprep.res = dados_synth, 
          Ylab = "Ocupação", 
          Xlab = "Ano", 
          Legend = c("Santa Maria", "Sintético"))


dados_synth <- dataprep(
  foo = dados,                     # DataFrame com os dados
  predictors = c("en_sup", "fem", "negro", "pessoas"), # Variáveis preditoras
  predictors.op = "mean",               # Média das variáveis preditoras
  dependent = "informal",             # Variável dependente
  unit.variable = "reg_id",            # Nome da coluna identificadora das RAs
  time.variable = "ano",                # Nome da coluna com o tempo
  treatment.identifier = "Santa Maria",        # RA tratada (por exemplo, Santa Maria)
  controls.identifier = c("Recanto Das Emas", "Planaltina", "Taguatinga",
                          "Brazlândia","Guará","Samambaia","Sobradinho",
                          "Núcleo Bandeirante","Candangolândia","Ceilândia"), # RAs de controle
  time.predictors.prior = 2009:2013,    # Período pré-tratamento
  time.optimize.ssr = 2009:2013,        # Período usado para otimizar os pesos
  unit.names.variable = "reg",      # Nome das unidades
  time.plot = 2009:2019                 # Período total de análise
)

# Executando o controle sintético
modelo_synth <- synth(data.prep.obj = dados_synth)

# Resultados
synth.tables <- synth.tab(dataprep.res = dados_synth, synth.res = modelo_synth)
print(synth.tables)


path.plot(synth.res = modelo_synth, dataprep.res = dados_synth, 
          Ylab = "Informal", 
          Xlab = "Ano", 
          Legend = c("Santa Maria", "Sintético"))


dados_synth <- dataprep(
  foo = dados,                     # DataFrame com os dados
  predictors = c("en_sup", "fem", "negro", "pessoas"), # Variáveis preditoras
  predictors.op = "mean",               # Média das variáveis preditoras
  dependent = "horas",             # Variável dependente
  unit.variable = "reg_id",            # Nome da coluna identificadora das RAs
  time.variable = "ano",                # Nome da coluna com o tempo
  treatment.identifier = "Santa Maria",        # RA tratada (por exemplo, Santa Maria)
  controls.identifier = c("Recanto Das Emas", "Planaltina", "Taguatinga",
                          "Brazlândia","Guará","Samambaia","Sobradinho",
                          "Núcleo Bandeirante","Candangolândia","Ceilândia"), # RAs de controle
  time.predictors.prior = 2009:2013,    # Período pré-tratamento
  time.optimize.ssr = 2009:2013,        # Período usado para otimizar os pesos
  unit.names.variable = "reg",      # Nome das unidades
  time.plot = 2009:2019                 # Período total de análise
)

# Executando o controle sintético
modelo_synth <- synth(data.prep.obj = dados_synth)

# Resultados
synth.tables <- synth.tab(dataprep.res = dados_synth, synth.res = modelo_synth)
print(synth.tables)


path.plot(synth.res = modelo_synth, dataprep.res = dados_synth, 
          Ylab = "Horas Trabalhadas", 
          Xlab = "Ano", 
          Legend = c("Santa Maria", "Sintético"))
