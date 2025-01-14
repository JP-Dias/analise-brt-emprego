# Pacotes
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plm)
library(stargazer)
library(lubridate)
library(sidrar)
library(fixest)
library(zoo)
library(stargazer)
library(modelsummary)

# Leitura da base_gama de dados
dados <- readRDS("analysis/dados/base_ra.RDS") #|> filter(reg %in% c("Gama","Santa Maria","Recanto Das Emas","Sobradinho"))


dados2 <- subset(dados, reg %in% c("Gama", "Brazlândia"))

dados3 <- subset(dados, reg %in% c("Santa Maria", "Recanto Das Emas"))

dados3 <- subset(dados3, !ano %in% c(2009, 2018, 2019))
dados2 <- subset(dados2, !ano %in% c(2009, 2018, 2019))


### Create a Variable of Interest ###

dados3$BRT_Effect <- ifelse(dados3$aamm > "201406" & dados3$reg == "Santa Maria", 1, 0)
dados3$Treat <- ifelse(dados3$reg == "Santa Maria", 1, 0)

dados2$BRT_Effect <- ifelse(dados2$aamm > "201406" & dados2$reg == "Gama", 1, 0)
dados2$Treat <- ifelse(dados2$reg == "Gama", 1, 0)



dados3$BRT_Effect <- ifelse(dados3$aamm > "201406" & dados3$reg == "Santa Maria", 1, 0)
dados3$Treat <- ifelse(dados3$reg == "Santa Maria", 1, 0)

reg6 <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2  + Treat|  aamm + fem + cor + escol + posicao_fam + setor_atv, ~conglom,weights = ~peso, data=dados3)



periodos_placebo <- unique(dados3$aamm[dados3$aamm < "201406"])


resultados_placebo <- data.frame(Periodo = character(), Coef = numeric(), Lower = numeric(), Upper = numeric(), stringsAsFactors = FALSE)


for (periodo in periodos_placebo) {

  dados3$BRT_Effect <- ifelse(dados3$aamm > periodo & dados3$reg == "Santa Maria", 1, 0)
  

  modelo <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2  | reg + aamm + fem + cor + escol + posicao_fam + setor_atv + pessoas, 
                  ~conglom, weights = ~peso, data = dados3)
  
 
  coef <- coef(modelo)["BRT_Effect"]
  conf <- confint(modelo, parm = "BRT_Effect",level=0.99)
  lower <- conf["BRT_Effect", "0.5 %"]
  upper <- conf["BRT_Effect", "99.5 %"]
  
 
  resultados_placebo <- resultados_placebo %>%
    add_row(Periodo = as.character(periodo), Coef = coef, Lower = lower, Upper = upper)
}


resultados_placebo <- resultados_placebo %>% arrange(Periodo)


ggplot(resultados_placebo, aes(x = as.Date(paste0(Periodo, "01"), "%Y%m%d"), y = Coef)) +
  geom_point() +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "",
       x = "Ano-Mês (Placebo)", y = "Coeficiente (Efeito BRT)") +
  theme_minimal()

confint(reg6, parm = "BRT_Effect",level=0.99)


resultados_placebo <- data.frame(Periodo = character(), Coef = numeric(),
                                 Lower_95 = numeric(), Lower_99 = numeric(),
                                 Upper_95 = numeric(), Upper_99 = numeric(),
                                 stringsAsFactors = FALSE)


# Loop com intervalos de confiança de 95% e 99%
for (periodo in periodos_placebo) {
  
  # Definindo a variável de tratamento
  dados3$BRT_Effect <- ifelse(dados3$aamm > periodo & dados3$reg == "Santa Maria", 1, 0)
  
  # Modelo com feols
  modelo <- feols(log(rend_bruto) ~ BRT_Effect + idade + idade2  | 
                    reg + aamm + fem + cor + escol + posicao_fam + setor_atv , 
                  ~conglom, weights = ~peso, data = dados3)
  
  # Coeficiente e intervalos de confiança
  coef <- coef(modelo)["BRT_Effect"]
  conf_95 <- confint(modelo, parm = "BRT_Effect", level = 0.95)
  conf_99 <- confint(modelo, parm = "BRT_Effect", level = 0.99)
  
  lower_95 <- conf_95["BRT_Effect", "2.5 %"]
  upper_95 <- conf_95["BRT_Effect", "97.5 %"]
  lower_99 <- conf_99["BRT_Effect", "0.5 %"]
  upper_99 <- conf_99["BRT_Effect", "99.5 %"]
  
  # Armazenando os resultados
  resultados_placebo <- resultados_placebo %>%
    add_row(Periodo = as.character(periodo), 
            Coef = coef, 
            Lower_95 = lower_95, Upper_95 = upper_95,
            Lower_99 = lower_99, Upper_99 = upper_99)
}

# Ordenando os resultados
resultados_placebo <- resultados_placebo %>% arrange(Periodo)

# Plot com ggplot2
ggplot(resultados_placebo, aes(x = as.Date(paste0(Periodo, "01"), "%Y%m%d"), y = Coef)) +
  geom_errorbar(aes(ymin = Lower_99, ymax = Upper_99), width = 10, color = "blue", alpha = 0.5) + # IC 99%
  geom_errorbar(aes(ymin = Lower_95, ymax = Upper_95), width = 10, color = "darkgreen", alpha = 0.8) + # IC 95%
  geom_point() + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Efeito Placebo do BRT com Intervalos de Confiança",
       x = "Ano-Mês (Placebo)", y = "Coeficiente (Efeito BRT)") +
  #scale_color_manual(values = c("blue", "darkgreen")) +
  theme_minimal()
