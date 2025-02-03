#install.packages("Synth")
library(Synth)
library(haven)
library(zoo)
library(Hmisc)

# Leitura da base de dados
base <- readRDS("analysis/dados/base_ra.RDS") 

# Criação da Variável de Efeito ----

## Gama ----
dados <- base |> 
  filter(!is.na(ocupado), idade %in% c(18:64), mora_mesma_ra == 1) |>  # Filtra ocupados, entre 18 e 64 anos e que moram na mesma RA nos últimos 12 meses 
  mutate(painel = case_when(mes %in% c(1,4,7,10)~"A",
                            mes %in% c(2,5,8,11)~"B",
                            TRUE~"C")) # Cria uma variável que sinaliza o Painel (A PED é feita em 3 paineis rotativos)

dados <- fastDummies::dummy_cols(dados,"escol")
dados <- fastDummies::dummy_cols(dados,"cor")

dados1 <- dados |> 
  mutate(idoso = ifelse(idade>=60,1,0)) |> 
  group_by(reg,ano) |> 
  summarise(
    analf = wtd.mean(escol_analf,peso,na.rm = T),
    sup = wtd.mean(escol_sup_com,peso,na.rm = T),
    idoso = wtd.mean(idoso,peso,na.rm = T),
    informal = wtd.mean(informal, peso,na.rm = T),
    pessoas = wtd.mean(pessoas, peso,na.rm = T),
    fem = wtd.mean(fem,peso),
    branca = wtd.mean(cor_branca,peso),
    preta = wtd.mean(cor_preta,peso),
    parda = wtd.mean(cor_parda,peso),
    rend_bruto = wtd.mean(rend_bruto,peso, na.rm = T),
    ocupado = wtd.mean(ocupado,peso,na.rm = T),
    horas = wtd.mean(horas_trab,peso,na.rm = T)
            ) |> 
  ungroup() |> 
  mutate(reg_id = as.numeric(as.factor(reg))) |> 
  as.data.frame() |> na.omit()

str(dados1)

dados_synth <- dataprep(
  foo = dados1,                     # DataFrame com os dados
  predictors = c("fem","branca","preta","parda","idoso","analf","sup","horas","informal","pessoas"), # Variáveis preditoras
  predictors.op = "mean",               # Média das variáveis preditoras
  dependent = "rend_bruto",             # Variável dependente
  unit.variable = "reg_id",            # Nome da coluna identificadora das RAs
  time.variable = "ano",                # Nome da coluna com o tempo
  treatment.identifier = "Santa Maria",        # RA tratada (por exemplo, Santa Maria)
  controls.identifier = c(
    "Taguatinga","Ceilândia","Brazlândia",        
    "Sobradinho","Planaltina","Paranoá","Núcleo Bandeirante","Guará",
    "Cruzeiro","Samambaia","São Sebastião",
    "Recanto Das Emas","Lago Sul","Riacho Fundo","Lago Norte","Candangolândia"
                          ), # RAs de controle
  time.predictors.prior = 2010:2013,    # Período pré-tratamento
  time.optimize.ssr = 2010:2013,        # Período usado para otimizar os pesos
  unit.names.variable = "reg",      # Nome das unidades
  time.plot = 2010:2018                 # Período total de análise
)

# Executando o controle sintético
modelo_synth <- synth(data.prep.obj = dados_synth)

# Resultados
synth.tables <- synth.tab(dataprep.res = dados_synth, synth.res = modelo_synth)
print(synth.tables)


path.plot(synth.res = modelo_synth, dataprep.res = dados_synth, 
          Ylab = "Rendimentos Brutos", 
          Xlab = "", 
          Legend = c("Santa Maria", "Sintético"))




