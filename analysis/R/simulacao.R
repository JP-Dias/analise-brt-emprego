library(fixest)
library(dplyr)
library(ggplot2)

set.seed(123)  # Para reprodutibilidade
n_sim <- 1000  # Número de simulações

# Criar uma lista para armazenar os resultados
df_results <- data.frame(simulation = integer(), coef = numeric(), p_value = numeric())

for (i in 1:n_sim) {
  dados3 <- dados3 %>%
    group_by(ano) %>%
    mutate(
      Treat_Placebo = sample(Treat, size = n()*.4, replace = FALSE),  # Seleciona tratados aleatoriamente
      BRT_Effect_Placebo = ifelse(Treat_Placebo == 1, 1, 0)  # Define o efeito placebo
    ) %>%
    ungroup()
  
  # Rodar a regressão com o placebo
  model_placebo <- feols(log(rend_bruto) ~ BRT_Effect_Placebo + idade + idade2  |  reg + aamm + fem + cor + escol + posicao_fam + setor_atv, ~reg, weights = ~peso, data = dados3)
  
  # Salvar coeficiente e p-valor
  coef_placebo <- coef(model_placebo)["BRT_Effect_Placebo"]
  p_value_placebo <- summary(model_placebo)$coeftable["BRT_Effect_Placebo", "Pr(>|t|)"]
  
  df_results <- rbind(df_results, data.frame(simulation = i, coef = coef_placebo, p_value = p_value_placebo))
}

# Gráfico da distribuição dos coeficientes placebo
p1 <- ggplot(df_results, aes(x = coef)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = coef(reg3)["BRT_Effect"], color = "red", linetype = "dashed", size = 1) +
  labs(title = "Distribuição dos Coeficientes Placebo", x = "Coeficiente do efeito placebo", y = "Frequência")

# Gráfico da distribuição dos p-valores
p2 <- ggplot(df_results, aes(x = p_value)) +
  geom_histogram(bins = 30, fill = "lightgreen", color = "black", alpha = 0.7) +
  labs(title = "Distribuição dos P-valores", x = "P-valor", y = "Frequência")

# Exibir os gráficos
print(p1)
print(p2)
