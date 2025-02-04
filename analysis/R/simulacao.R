library(fixest)
library(dplyr)
library(ggplot2)

dados3 <- dados |>
  filter(
    reg %in% c("Santa Maria", "Recanto Das Emas"),
    !(ano %in% c(2009,2018:2019))) |> 
  mutate(BRT_Effect = ifelse(aamm > "201406" & reg == "Santa Maria", 1, 0),
         Treat = ifelse(reg == "Santa Maria", 1, 0))

reg3 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2  |  reg + aamm + fem + cor + escol + posicao_fam + setor_atv, ~reg,weights = ~peso, data=dados3)
library(furrr)
library(fixest)
library(dplyr)
library(tictoc)

set.seed(123)  # Para reprodutibilidade

# Configurar paralelização
plan(multisession)  # Usa múltiplos núcleos do processador

# Criar a função para rodar a simulação individual
simulacao_placebo <- function(i) {
  dados3 <- dados3 %>%
    group_by(aamm) %>%
    mutate(
      Treat_Placebo = sample(c(1, 0), prob = c(.4, .6), size = n(), replace = TRUE),
      BRT_Effect_Placebo = ifelse(aamm > "201406" & Treat_Placebo == 1, 1, 0)
    ) %>%
    ungroup()
  
  # Rodar a regressão com o placebo
  model_placebo <- try(
    feols(log(rend_bruto) ~ BRT_Effect_Placebo + idade + idade2  |  
            Treat_Placebo + aamm + fem + cor + escol + posicao_fam + setor_atv,
          ~reg, weights = ~peso, data = dados3),
    silent = TRUE
  )
  
  # Verifica se o modelo foi estimado corretamente e se a variável BRT_Effect_Placebo está presente
  if (inherits(model_placebo, "fixest") && "BRT_Effect_Placebo" %in% names(coef(model_placebo))) {
    
    coef_placebo <- coef(model_placebo)["BRT_Effect_Placebo"]
    p_value_placebo <- summary(model_placebo)$coeftable["BRT_Effect_Placebo", "Pr(>|t|)"]
    
    return(data.frame(simulation = i, coef = coef_placebo, p_value = p_value_placebo))
  } else {
    return(NULL)  # Retorna NULL se a regressão falhar
  }
}

# Número de simulações
n_sim <- 100000

# Iniciar o cronômetro
tic("Execução paralela")

# Executar as simulações em paralelo
df_results_list <- future_map(1:n_sim, simulacao_placebo, .progress = TRUE)

# Combinar os resultados válidos em um único data frame
df_results <- bind_rows(df_results_list)

# Finalizar o cronômetro
toc()

# Exibir resumo dos coeficientes estimados
summary(df_results$coef)

# Gráfico da distribuição dos coeficientes placebo
p1 <- ggplot(df_results, aes(x = coef)) +
  geom_histogram(bins = 150, fill = "lightyellow", color = "black", alpha = 0.7) +
  geom_vline(xintercept = coef(reg3)["BRT_Effect"], color = "red", linetype = "dashed", size = 1) +
  labs(title = "Distribuição dos Coeficientes Placebo", x = "Coeficiente do efeito placebo", y = "Frequência")
 
p1



p1 <- ggplot(df_results, aes(x = coef)) +
  geom_vline(xintercept = 0, color = "black", size = .5, , alpha = 0.5) +
  geom_histogram(bins = 200, fill = "lightyellow", color = "black", alpha = 0.7) + 
  theme_test() +
  geom_vline(xintercept = coef(reg3)["BRT_Effect"], color = "red", linetype = "dashed") +
  labs(title = "", x = "Coeficientes", y = "Frequência") + 
  scale_y_continuous(expand = c(.02,0.1))

p1 <- ggplot(df_results, aes(x = coef)) +
  # Adicionar histograma com nome na legenda
  geom_histogram(bins = 100, aes(fill = "Coef. Placebo"), color = "black", alpha = 0.8) + 
  # Linha preta central
  geom_vline(xintercept = 0, color = "black", size = 0.5, alpha = 0.5) +
  # Linha vermelha com nome na legenda
  geom_vline(aes(xintercept = coef(reg3)["BRT_Effect"], color = "Coef. Modelo Principal"), linetype = "dashed") +
  # Temas e rótulos
  theme_test() +
  labs(title = "", x = "", y = "Frequência", fill = "", color = "") + 
  scale_y_continuous(expand = c(.02, 0.1)) +
  # Definir cores na legenda
  scale_fill_manual(values = c("Coef. Placebo" = "lightyellow")) +
  scale_color_manual(values = c("Coef. Modelo Principal" = "red")) +
  theme(legend.position = c(0.1, 0.8)) 


 
p1

# Gráfico da distribuição dos p-valores
p2 <- ggplot(df_results, aes(x = p_value)) +
  geom_histogram(bins = 100, fill = "lightgreen", color = "black", alpha = 0.7) +
  labs(title = "Distribuição dos P-valores", x = "P-valor", y = "Frequência")

p2
