library("fixest")
library("dplyr")
library("HonestDiD")


#### Open Data ####

pasta <- "C:/Users/ricar/Desktop/Efeitos BRT/Dados e Estimativas/Results"

setwd(pasta)

dados <- readRDS("base_ra.RDS")

### Remove the years after 2018 ###

dados2 <- subset(dados, 
                 reg %in% c("Santa Maria", "Recanto Das Emas") & 
                   !ano %in% c(2018, 2019))


#### Create new Dataset #####

dados_agg <- dados2 %>%
  group_by(reg, ano, aamm) %>%
  summarise(
    rend_bruto = sum(rend_bruto, na.rm = TRUE),
    informal = sum(informal, na.rm = TRUE),
    horas_trab = sum(horas_trab, na.rm = TRUE),
    trab_plano = sum(trab_plano, na.rm = TRUE),
    mora_mesma_ra = sum(mora_mesma_ra, na.rm = TRUE),
    analf = sum((escol %in% c("analf")), na.rm = TRUE),
    educ_prim = sum((escol %in% c("fund_com", "med_inc")), na.rm = TRUE),
    educ_sec = sum((escol %in% c("med_com", "sup_inc")), na.rm = TRUE),
    educ_terc = sum((escol %in% c("sup_com")), na.rm = TRUE),
    preta_parda = sum((cor %in% c("preta", "parda")), na.rm = TRUE),
    setor_serv_comerc = sum((setor_atv %in% c("servic", "comerc")), na.rm = TRUE),
    setor_setor_ind = sum((setor_atv %in% c("indust", "construc")), na.rm = TRUE),
    fem = sum((fem == 1), na.rm = TRUE),
    n_trabalhadores = n(),
    media_dom = mean(pessoas, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    renda_pc = rend_bruto / n_trabalhadores
  ) %>%
  mutate(
    across(
      c(informal, trab_plano, mora_mesma_ra, analf, educ_prim, educ_sec, educ_terc,
        preta_parda, setor_serv_comerc, setor_setor_ind, fem),
      ~ .x / n_trabalhadores,
      .names = "prop_{.col}"
    )
  )




saveRDS(dados_agg, file = "dados_agg_reg.rds")



