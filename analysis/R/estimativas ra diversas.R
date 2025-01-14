# Pacotes
library(tidyverse)
library(haven)
library(plm)
library(stargazer)
library(lubridate)
library(sidrar)
library(fixest)
library(zoo)
library(stargazer)
library(modelsummary)
library(fastDummies)

# ```

# Construção da Base

# ```{r bases}

# Leitura da base de dados
base <- readRDS("analysis/dados/base_ra.RDS") |> filter(reg %in% c("Gama","Santa Maria","Recanto Das Emas","Sobradinho")) 

# Criação da Variável de Efeito ----

## Gama ----
dados <- base |> 
  mutate(grupo = ifelse(reg %in% c("Gama","Santa Maria"),1,0), # Monta a variável de Tratamento
         Effect = grupo * intervencao) |> # Montar Variável de Interesse
  filter(!is.na(ocupado), idade %in% c(18:64),
         mora_mesma_ra == 1,
         #trab_plano == 1
         ) |>  # Filtra ocupados, entre 18 e 64 anos e que moram na mesma RA nos últimos 12 meses 
  mutate(painel = case_when(mes %in% c(1,4,7,10)~"A",
                            mes %in% c(2,5,8,11)~"B",
                            TRUE~"C")) # Cria uma variável que sinaliza o Painel (A PED é feita em 3 paineis rotativos)

dados_gama <- dados |> filter(reg %in% c("Gama","Sobradinho"))
dados_sm <- dados |> filter(reg %in% c("Santa Maria","Recanto Das Emas"))

# ```
# Estimativas


# Gama
reg1_gama <- feols(c(ln_rend_bruto,ocupado,informal,horas_trab,ln_rend_liquido) ~ Effect  | aamm + reg, se = "cluster",cluster = "conglom", data = dados_gama)
reg2_gama <- feols(c(ln_rend_bruto,ocupado,informal,horas_trab,ln_rend_liquido) ~ Effect + en_sup + fem + negro + pessoas | aamm + reg, se = "cluster",cluster = "conglom", data = dados_gama)

# Santa Maria
reg1_sm <- feols(c(ln_rend_bruto,ocupado,informal,horas_trab,ln_rend_liquido) ~ Effect  | aamm + reg, se = "cluster",cluster = "conglom", data = dados_sm)
reg2_sm <- feols(c(ln_rend_bruto,ocupado,informal,horas_trab,ln_rend_liquido) ~ Effect  + en_sup + fem + negro + pessoas| aamm + reg, se = "cluster",cluster = "conglom", data = dados_sm)

# Geral
reg1_geral <- feols(c(ln_rend_bruto,ocupado,informal,horas_trab,ln_rend_liquido) ~ Effect  | aamm + reg, se = "cluster",cluster = "conglom", data = dados)
reg2_geral <- feols(c(ln_rend_bruto,ocupado,informal,horas_trab,ln_rend_liquido) ~ Effect + en_sup + fem + negro + pessoas | aamm + reg, se = "cluster",cluster = "conglom", data = dados)

# Rendimento Bruto
modelsummary(list(
  "Gama | Sem controles"=reg1_gama[1],
  "Gama | Com controles"=reg2_gama[1],
  "Santa Maria | Sem controles"=reg1_sm[1],
  "Santa Maria | Com controles"=reg2_sm[1],
  "Geral | Sem controles"=reg1_geral[1],
  "Geral | Com controles"=reg2_geral[1]
  ),
  output ="default",
  stars = T)

# Ocupado
modelsummary(list(
  "Gama | Sem controles"=reg1_gama[2],
  "Gama | Com controles"=reg2_gama[2],
  "Santa Maria | Sem controles"=reg1_sm[2],
  "Santa Maria | Com controles"=reg2_sm[2],
  "Geral | Sem controles"=reg1_geral[2],
  "Geral | Com controles"=reg2_geral[2]
),
output ="default",
stars = T)

# Informal
modelsummary(list(
  "Gama | Sem controles"=reg1_gama[3],
  "Gama | Com controles"=reg2_gama[3],
  "Santa Maria | Sem controles"=reg1_sm[3],
  "Santa Maria | Com controles"=reg2_sm[3],
  "Geral | Sem controles"=reg1_geral[3],
  "Geral | Com controles"=reg2_geral[3]
),
output ="default",
stars = T)

# Horas Trabalhadas
modelsummary(list(
  "Gama | Sem controles"=reg1_gama[4],
  "Gama | Com controles"=reg2_gama[4],
  "Santa Maria | Sem controles"=reg1_sm[4],
  "Santa Maria | Com controles"=reg2_sm[4],
  "Geral | Sem controles"=reg1_geral[4],
  "Geral | Com controles"=reg2_geral[4]
),
output ="default",
stars = T)

# Rendimento Líquido
modelsummary(list(
  "Gama | Sem controles"=reg1_gama[5],
  "Gama | Com controles"=reg2_gama[5],
  "Santa Maria | Sem controles"=reg1_sm[5],
  "Santa Maria | Com controles"=reg2_sm[5],
  "Geral | Sem controles"=reg1_geral[5],
  "Geral | Com controles"=reg2_geral[5]
),
output ="default",
stars = T)

# Event Study

# Rendimento Bruto
# Gama
iplot(feols(ln_rend_bruto ~ i(ano, grupo, 2013) | aamm + reg, se = "cluster", cluster = "conglom", data = dados_gama),col = "darkblue", sub = "Efeitos fixos reg e ano")
iplot(feols(ln_rend_bruto ~ i(ano, grupo, 2013) + en_sup + fem + negro + pessoas | aamm + reg , se = "cluster",cluster = "conglom", data = dados_gama),col = "darkblue", sub = "Efeitos fixos reg e ano-mês")

iplot(feols(ocupado ~ i(ano, grupo, 2013) | aamm + reg, se = "cluster", cluster = "conglom", data = dados_gama),col = "darkblue", sub = "Efeitos fixos reg e ano")
iplot(feols(ocupado ~ i(ano, grupo, 2013) + en_sup + fem + negro + pessoas | aamm + reg , se = "cluster",cluster = "conglom", data = dados_gama),col = "darkblue", sub = "Efeitos fixos reg e ano-mês")

iplot(feols(informal ~ i(ano, grupo, 2013) | aamm + reg, se = "cluster", cluster = "conglom", data = dados_gama),col = "darkblue", sub = "Efeitos fixos reg e ano")
iplot(feols(informal ~ i(ano, grupo, 2013) + en_sup + fem + negro + pessoas | aamm + reg , se = "cluster",cluster = "conglom", data = dados_gama),col = "darkblue", sub = "Efeitos fixos reg e ano-mês")


# Santa Maria
iplot(feols(ln_rend_bruto ~ i(ano, grupo, 2013) | aamm + reg, se = "cluster", cluster = "conglom", data = dados_sm),col = "darkblue", sub = "Efeitos fixos reg e ano")
iplot(feols(ln_rend_bruto ~ i(ano, grupo, 2013) + en_sup + fem + negro + pessoas | aamm + reg , se = "cluster",cluster = "conglom", data = dados_sm),col = "darkblue", sub = "Efeitos fixos reg e ano")


# Geral
iplot(feols(ln_rend_bruto ~ i(ano, grupo, 2013) | aamm + reg, se = "cluster", cluster = "conglom", data = dados),col = "darkblue", sub = "Efeitos fixos reg e ano")
iplot(feols(ln_rend_bruto ~ i(ano, grupo, 2013) + en_sup + fem + negro + pessoas | aamm + reg , se = "cluster",cluster = "conglom", data = dados),col = "darkblue", sub = "Efeitos fixos reg e ano")

# Gama
iplot(feols(ln_rend_bruto ~ i(ano, grupo, 2013) | aamm + reg, se = "cluster", cluster = "conglom", data = dados_gama),col = "darkblue", sub = "Efeitos fixos reg e ano")
iplot(feols(ln_rend_bruto ~ i(ano, grupo, 2013) + en_sup + fem + negro + pessoas | aamm + reg , se = "cluster",cluster = "conglom", data = dados_gama),col = "darkblue", sub = "Efeitos fixos reg e ano")

# Santa Maria
iplot(feols(ln_rend_bruto ~ i(ano, grupo, 2013) | aamm + reg, se = "cluster", cluster = "conglom", data = dados_sm),col = "darkblue", sub = "Efeitos fixos reg e ano")
iplot(feols(ln_rend_bruto ~ i(ano, grupo, 2013) + en_sup + fem + negro + pessoas | aamm + reg , se = "cluster",cluster = "conglom", data = dados_sm),col = "darkblue", sub = "Efeitos fixos reg e ano")


# Geral
iplot(feols(ln_rend_bruto ~ i(ano, grupo, 2013) | aamm + reg, se = "cluster", cluster = "conglom", data = dados),col = "darkblue", sub = "Efeitos fixos reg e ano")
iplot(feols(ln_rend_bruto ~ i(ano, grupo, 2013) + en_sup + fem + negro + pessoas | aamm + reg , se = "cluster",cluster = "conglom", data = dados),col = "darkblue", sub = "Efeitos fixos reg e ano")



modelsummary(list(),output ="markdown",
             stars = T)



# ```{r ef anomes sc sm}
feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo | aamm, data = dados_sm)

# ```

### Efeito Fixo de mês

# ```{r ef mes sc gama}
feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo | mes, data = dados_gama)

# ```

# ```{r ef mes sc sm}
feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo | mes, data = dados_sm)

# ```

### Efeito Fixo de conglom

# ```{r ef conglom sc gama}
feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo | conglom, data = dados_gama)

# ```

# ```{r ef conglom sc sm}
feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo | conglom, data = dados_sm)

# ```

### Efeito Fixo de RA

# ```{r ef reg sc gama}
feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo | reg, data = dados_gama)

# ```

# ```{r ef reg sc sm}
feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo | reg, data = dados_sm)

# ```

### Efeito Fixo de RA e ano mes

# ```{r ef reg anomes sc gama}
feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo | reg + aamm, data = dados_gama)

feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo | reg^aamm, data = dados_gama)

# ```

# ```{r ef reg anomes sc sm}
feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo | reg + aamm, data = dados_sm)

feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo | reg^aamm, data = dados_sm)

# ```

### Efeito Fixo de RA e mes

# ```{r ef reg mes sc gama}
feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo | reg + mes, data = dados_gama)

feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo | reg^mes, data = dados_gama)

# ```

# ```{r ef reg mes sc sm}
feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo | reg + mes, data = dados_sm)

feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo | reg^mes, data = dados_sm)

# ```

### Efeito Fixo de conglom e mes

# ```{r ef conglom mes sc gama}
feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo | conglom + mes, data = dados_gama)

feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo | conglom^mes, data = dados_gama)

# ```

# ```{r ef conglom mes sc sm}
feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo | conglom + mes, data = dados_sm)

feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo | conglom^mes, data = dados_sm)

# ```


## Modelo com controles


### Efeito Fixo de ano-mês

# ```{r ef anomes cc gama}
feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo + en_sup + fem + negro + pessoas | aamm, data = dados_gama)

# ```

# ```{r ef anomes cc sm}
feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo + en_sup + fem + negro + pessoas | aamm, data = dados_sm)

# ```

### Efeito Fixo de mês

# ```{r ef mes cc gama}
feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo + en_sup + fem + negro + pessoas | mes, data = dados_gama)

# ```

# ```{r ef mes cc sm}
feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo + en_sup + fem + negro + pessoas | mes, data = dados_sm)

# ```

### Efeito Fixo de conglom

# ```{r ef conglom cc gama}
feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo + en_sup + fem + negro + pessoas | conglom, data = dados_gama)

# ```

# ```{r ef conglom cc sm}
feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo + en_sup + fem + negro + pessoas | conglom, data = dados_sm)

# ```

### Efeito Fixo de RA

# ```{r ef reg cc gama}
feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo + en_sup + fem + negro + pessoas | reg, data = dados_gama)

# ```

# ```{r ef reg cc sm}
feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo + en_sup + fem + negro + pessoas | reg, data = dados_sm)

# ```

### Efeito Fixo de RA e ano mes

# ```{r ef reg anomes cc gama}
feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo + en_sup + fem + negro + pessoas | reg + aamm, data = dados_gama)

feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo + en_sup + fem + negro + pessoas | reg^aamm, data = dados_gama)

# ```

# ```{r ef reg anomes cc sm}
feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo + en_sup + fem + negro + pessoas | reg + aamm, data = dados_sm)

feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo + en_sup + fem + negro + pessoas | reg^aamm, data = dados_sm)

# ```

### Efeito Fixo de RA e mes

# ```{r ef reg mes cc gama}
feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo + en_sup + fem + negro + pessoas | reg + mes, data = dados_gama)

feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo + en_sup + fem + negro + pessoas | reg^mes, data = dados_gama)

# ```

# ```{r ef reg mes cc sm}
feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo + en_sup + fem + negro + pessoas | reg + mes, data = dados_sm)

feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo + en_sup + fem + negro + pessoas | reg^mes, data = dados_sm)

# ```

### Efeito Fixo de conglom e mes

# ```{r ef conglom mes cc gama}
feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo + en_sup + fem + negro + pessoas | conglom + mes, data = dados_gama)

feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo + en_sup + fem + negro + pessoas | conglom^mes, data = dados_gama)

# ```

# ```{r ef conglom mes cc sm}
feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo + en_sup + fem + negro + pessoas | conglom + mes, data = dados_sm)

feols(c(ocupado,informal, ln_rend_bruto,ln_rend_liquido,ln_horas_trab) ~ Effect + intervencao + grupo + en_sup + fem + negro + pessoas | conglom^mes, data = dados_sm)

# ```

