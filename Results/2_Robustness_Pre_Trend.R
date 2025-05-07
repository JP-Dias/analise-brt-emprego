library("fixest")
library("dplyr")
library("HonestDiD")
library("showtext")
library("tidyverse")


#### Open Data ####

# pasta <- "C:/Users/ricar/Desktop/Efeitos BRT/Dados e Estimativas"
# setwd(pasta)
  
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


dados <- readRDS("base_RA.RDS")

#### Constructing a Subset for Treated and Control Areas ###

dados2 <- subset(dados, reg %in% c("Gama", "Sobradinho") & !ano %in% c(2018, 2019))
dados3 <- subset(dados, reg %in% c("Santa Maria", "Recanto Das Emas") & !ano %in% c(2018, 2019))

### Create a Variable of Interest ###

dados2$BRT_Effect <- ifelse(dados2$aamm > "201406" & dados2$reg == "Gama", 1, 0)
dados2$Treat <- ifelse(dados2$reg == "Gama", 1, 0)

dados3$BRT_Effect <- ifelse(dados3$aamm > "201406" & dados3$reg == "Santa Maria", 1, 0)
dados3$Treat <- ifelse(dados3$reg == "Santa Maria", 1, 0)

### Creating other Variables ###

dados2$horas_trabM <- dados2$horas_trab * 4
dados3$horas_trabM <- dados3$horas_trab * 4

### Event-Study - Santa Maria ###

attach(dados3)

reg1 <- feols(log(rend_bruto/horas_trabM) ~ i(ano, Treat, 2013) + idade | reg + aamm + escol + fem + cor + pessoas + setor_atv,
               ~conglom,
               data=dados3)

event_study_plot1 <- iplot(reg1)

font_add("cmunrm", regular = "cmunrm.ttf")
showtext_auto()

par(mar = c(3, 4, 1, 1), family = "cmunrm")

iplot(reg1, 
      pt.col = "darkblue", 
      ci.col = "#1f78b4", 
      pt.size = 1.5,
      ylab = "Estimate and 95% confidence interval",
      xlab = "",
      sub = "",
      main = "")



reg2 <- feols(ocupado ~ i(ano, Treat, 2013) + idade | reg + aamm + escol + fem + cor + pessoas,
              ~conglom,
              data=dados3)

event_study_plot2 <- iplot(reg2)

par(mar = c(3, 4, 1, 1), family = "cmunrm")

iplot(reg2, 
      pt.col = "darkblue", 
      ci.col = "#1f78b4", 
      pt.size = 1.5,
      ylab = "Estimate and 95% confidence interval",
      xlab = "",
      sub = "",
      main = "")



### HonestDiD - Santa Maria - Earnings ###

betahat <- summary(reg1)$coefficients
sigma <- summary(reg1)$cov.scaled

betahat <- betahat[-c(9)]
sigma <- sigma[-c(9), -c(9)]

delta_rm_results_earnings <-
  createSensitivityResults_relativeMagnitudes(
    betahat = betahat,
    sigma = sigma,
    numPrePeriods = 4, 
    numPostPeriods = 4, 
    Mbarvec = seq(0.1,0.4,by=0.05),
    l_vec = basisVector(2,4) 
  )


originalResults <- constructOriginalCS(betahat = betahat,
                                                  sigma = sigma,
                                                  numPrePeriods = 4,
                                                  numPostPeriods = 4,
                                                  l_vec = basisVector(2,4))


honest_plot1 <- createSensitivityPlot_relativeMagnitudes(delta_rm_results_earnings, originalResults)


honest_plot1 <- honest_plot1 +
  theme_bw(base_family = "cmunrm",base_size = 15) +
  labs(
    title = "",
    x = "M",
    y = "95% confidence interval",
    color = ""
  ) +
  scale_color_manual(values = c("#1f78b4","darkblue"))

### HonestDiD - Santa Maria - Employment ###

betahat2 <- summary(reg2)$coefficients
sigma2 <- summary(reg2)$cov.scaled

betahat2 <- betahat2[-c(9)]
sigma2 <- sigma2[-c(9), -c(9)]

delta_rm_results_employment <-
  createSensitivityResults_relativeMagnitudes(
    betahat = betahat2,
    sigma = sigma2,
    numPrePeriods = 4, 
    numPostPeriods = 4, 
    Mbarvec = seq(0.1,0.4,by=0.05),
    l_vec = basisVector(2,4) 
  )


originalResults2 <- constructOriginalCS(betahat = betahat2,
                                       sigma = sigma2,
                                       numPrePeriods = 4,
                                       numPostPeriods = 4,
                                       l_vec = basisVector(2,4))


honest_plot2 <- createSensitivityPlot_relativeMagnitudes(delta_rm_results_employment, originalResults2)

honest_plot2 <- honest_plot2 +
  theme_bw(base_family = "cmunrm",base_size = 15) +
  labs(
    title = "",
    x = "M",
    y = "95% confidence interval",
    color = ""
  ) +
  scale_color_manual(values = c("#1f78b4","darkblue"))

#### Saving the Figures #####












