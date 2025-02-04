library("fixest")
library("dplyr")
library("MatchIt")


source("config.R")

options(scipen=99999)

`%notin%` <- negate(`%in%`)

dados <- readRDS(analysis("dados/dados_controle_sintetico.RDS"))


dados2 <- subset(dados, ano %in% c(2013)) 

#dados2 <- fastDummies::dummy_cols(dados2,"ano")

dados3 <- subset(dados2, reg %notin% c("Santa Maria","Sobradinho","Plano Piloto","Lago Norte","Lago Sul","Cruzeiro"))

dados3 <- dados3 |> mutate(Gama_Treat=ifelse(reg == "Gama", 1, 0))

PSM <- matchit(Gama_Treat ~ perc_analf + perc_idoso + perc_informal + perc_mora_trabalha + 
                 perc_sup + media_familia + media_idade + perc_nasc_df + 
                 #ano_2010 + ano_2011 + ano_2012 +
                 log(renda_fam_media) + log(n_empregos) + log(massa_salarial) + log(densidade_2010), 
               data = dados3, 
               method = "nearest",
               link = "logit")
PSM
match.data(PSM)

m.data1 <- match.data(PSM)

ordem <- c("Treated","Nearest", "Second\nNearest","Third\nNearest")

dados3 |> 
  mutate(
    log_renda_fam_media = log(renda_fam_media),
    log_n_empregos = log(n_empregos),
    log_massa_salarial = log(massa_salarial),
    log_densidade_2010 = log(densidade_2010)
                          ) |> 
  filter(reg %in% c("Gama","Ceilândia","Guará","Brazlândia")) |> 
  mutate(cor = case_when(
    reg == "Gama"~"Treated",
    reg == "Ceilândia"~"Nearest",
    reg == "Guará"~"Second\nNearest",
    reg == "Brazlândia"~"Third\nNearest"
                         ),
    cor = factor(cor, levels = ordem)) |> 
  select(cor, perc_idoso, perc_analf, perc_sup, perc_nasc_df, media_familia,
         perc_mora_trabalha, log_densidade_2010, perc_ocupado, perc_informal,
         log_renda_fam_media, log_n_empregos, log_massa_salarial, log_densidade_2010) |> 
  gather("var", "valor", c(3:14)) |> 
  ggplot(aes(x = cor, y = valor, fill = cor)) + 
  geom_col(color = "black", lwd = 0.5) +
  facet_wrap(~var, scales = "free", 
             labeller = labeller(var = c(
               perc_idoso         = "% de Idosos",
               perc_analf         = "% de Analfabetos",
               perc_sup           = "% com Ensino Superior",
               perc_nasc_df       = "% Nascidos no DF",
               media_familia      = "Média tamanho Família",
               perc_mora_trabalha = "% que trabalham na Região",
               log_densidade_2010 = "Log da Densidade (2010)",
               perc_ocupado       = "% Ocupados",
               perc_informal      = "% Informais",
               log_renda_fam_media= "Log da Renda Familiar Média",
               log_n_empregos     = "Log do Número de Empregos",
               log_massa_salarial = "Log da Massa Salarial"
             ))) +
  theme_minimal() +
  scale_fill_manual(values = c("white", "grey80", "grey60", "grey40")) +
  theme(legend.position = "none") +
  labs(x = "", y = "", col = "", fill = "")

ggsave("psm_gama.pdf",width = 12, height = 9,dpi = 300)

##### Fixar Antes do Choque (2013) - PSM de Santa Maria #####l

dados2 <- subset(dados, ano %in% c(2012)) 

dados4 <- subset(dados2, dados2$reg %notin% c("Gama","Sobradinho","Plano Piloto","Lago Norte","Lago Sul","Cruzeiro"))

dados4$Santa_Maria_Treat <- ifelse(dados4$reg == "Santa Maria", 1, 0)

PSM2 <- matchit(Santa_Maria_Treat ~ perc_analf + perc_idoso + perc_informal + perc_mora_trabalha
                + log(n_empregos) + log(massa_salarial) + log(densidade_2010), 
                data = dados4, method = "optimal",link = "probit")

PSM2
summary(PSM2)
match.data(PSM2)

m.data2 <- match.data(PSM2)

m.data2



dados4 |> 
  mutate(
    log_renda_fam_media = log(renda_fam_media),
    log_n_empregos = log(n_empregos),
    log_massa_salarial = log(massa_salarial),
    log_densidade_2010 = log(densidade_2010)
  ) |> 
  filter(reg %in% c("Santa Maria","Recanto Das Emas","Riacho Fundo","Paranoá")) |> 
  mutate(cor = case_when(
    reg == "Santa Maria"~"Treated",
    reg == "Recanto Das Emas"~"Nearest",
    reg == "Riacho Fundo"~"Second\nNearest",
    reg == "Paranoá"~"Third\nNearest"
  ),
  cor = factor(cor, levels = ordem)) |> 
  select(cor,perc_idoso,perc_analf,perc_sup,perc_nasc_df, media_familia,
         perc_mora_trabalha,log_densidade_2010,perc_ocupado,perc_informal,
         log_renda_fam_media,log_n_empregos ,log_massa_salarial,log_densidade_2010) |> 
  gather("var","valor",c(3:14)) |> ggplot(aes(x = cor, y = valor, fill = cor)) + 
  geom_col(color = "black",lwd = .5) + facet_wrap(~var,scales = "free") + theme_minimal() +
  scale_fill_manual(values = c("white","grey80","grey60","grey40")) +
  theme(legend.position = "none") +
  labs(x = "",y = "", col = "", fill= "")

dados4 |> 
  mutate(
    log_renda_fam_media = log(renda_fam_media),
    log_n_empregos       = log(n_empregos),
    log_massa_salarial   = log(massa_salarial),
    log_densidade_2010   = log(densidade_2010)
  ) |> 
  filter(reg %in% c("Santa Maria","Recanto Das Emas","Riacho Fundo","Paranoá")) |> 
  mutate(cor = case_when(
    reg == "Santa Maria"      ~ "Treated",
    reg == "Recanto Das Emas" ~ "Nearest",
    reg == "Riacho Fundo"      ~ "Second\nNearest",
    reg == "Paranoá"           ~ "Third\nNearest"
  ),
  cor = factor(cor, levels = ordem)) |> 
  select(cor, perc_idoso, perc_analf, perc_sup, perc_nasc_df, media_familia,
         perc_mora_trabalha, log_densidade_2010, perc_ocupado, perc_informal,
         log_renda_fam_media, log_n_empregos, log_massa_salarial, log_densidade_2010) |> 
  gather("var", "valor", c(3:14)) |> 
  ggplot(aes(x = cor, y = valor, fill = cor)) + 
  geom_col(color = "black", lwd = 0.5) +
  facet_wrap(~var, scales = "free", 
             labeller = labeller(var = c(
               perc_idoso         = "% de Idosos",
               perc_analf         = "% de Analfabetos",
               perc_sup           = "% com Ensino Superior",
               perc_nasc_df       = "% Nascidos no DF",
               media_familia      = "Média tamanho Família",
               perc_mora_trabalha = "% que trabalham na Região",
               log_densidade_2010 = "Log da Densidade (2010)",
               perc_ocupado       = "% Ocupados",
               perc_informal      = "% Informais",
               log_renda_fam_media= "Log da Renda Familiar Média",
               log_n_empregos     = "Log do Número de Empregos",
               log_massa_salarial = "Log da Massa Salarial"
             ))) +
  theme_minimal() +
  scale_fill_manual(values = c("white", "grey80", "grey60", "grey40")) +
  theme(legend.position = "none") +
  labs(x = "", y = "", col = "", fill = "")

ggsave("psm_sm.pdf",width = 12, height = 9,dpi = 300)
