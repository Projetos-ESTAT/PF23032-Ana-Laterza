source("rdocs/source/packages.R")

# ---------------------------------------------------------------------------- #

#        ______   _____  ________      ________ 
#      |  ____| / ____| |__   __| /\  |__   __|
#     | |__    | (___     | |   /  \    | |   
#    |  __|    \___ \    | |  / /\ \   | |   
#   | |____   ____) |   | |  /____ \  | |   
#  |______   |_____/   |_| /_/    \_\|_|   
#  
#         Consultoria estatística 
#

# ---------------------------------------------------------------------------- #
# ############################## README ###################################### #
# Consultor, favor utilizar este arquivo .R para realizar TODAS as análises
# alocadas a você neste projeto pelo gerente responsável, salvo instrução 
# explícita do gerente para mudança.
#
# Escreva seu código da forma mais clara e legível possível, eliminando códigos
# de teste depreciados, ou ao menos deixando como comentário. Dê preferência
# as funções dos pacotes contidos no Tidyverse para realizar suas análises.
# ---------------------------------------------------------------------------- #
p_load(readxl,lubridate)

# 1.0) Leitura e pacotes ----
df <- read_excel("banco/análise-para-ESTAT.xlsx",
                 sheet = "respostas",
                 col_names = FALSE,
                 skip = 4,
                 na = "0")

cnames <- read_excel("banco/colnames.xlsx", 
                       col_names = FALSE)
df <- df[,4:83]
colnames(df) <- cnames
rm(cnames)

# 2.0) Ajustes ----

df <- df |>
  mutate(across(1, as.Date)) %>%  # Converte a primeira coluna em formato "as_date"
  mutate(across(-1, as.factor)) 

df$`n. desvios`[is.na(df$`n. desvios`)] <- 0

df <- df |>
  filter(!(`n. desvios` == 0 & is.na(`100% norma`)))

levels(df$`100% norma`) <- c("norma", "Não norma")
df$`100% norma`[is.na(df$`100% norma`)] <- factor("Não norma")

# removendo colunas desinteressantes:
df <- df[, -c(1,2,3,77,78,79,80)]

saveRDS(df, "banco/df.rds")
#df <- readRDS("banco/df.rds")

# --------------------------------------------------------------------------- #
# 3.0) Análise multivariada ----

p_load(FactoMineR, factoextra, amap, ade4, openxlsx,
       ggrepel, HH, likert, janitor, reshape2,RColorBrewer,
       plyr,psych,lavaan, semPlot, conover.test, ca,
       vcd,gplots)

a2 <- df %>% dplyr::select(1, 3)
a2[] <- lapply(a2, factor)
mca1_br1 <- ca::mjca(obj = a2, lambda = "Burt")
summary(mca1_br1)
mca2_br1 <- dudi.acm(a2, scannf = FALSE)
cats_br1 <- apply(a2, 2, function(x) nlevels(as.factor(x)))
mca2_br1_vars_df <- data.frame(mca2_br1$co,
                               Variable = rep(names(cats_br1), cats_br1))
dim1 <- str_c("Dimensão 1 (","",round(mca1_br1$inertia.e[1]*100,2),"", "%)")
dim2 <- str_c("Dimensão 2 (","",round(mca1_br1$inertia.e[2]*100,2),"", "%)")
ggplot(data = mca2_br1_vars_df,
       aes(x = Comp1, y = Comp2,
           label = rownames(mca2_br1_vars_df),
           col = Variable,
           size = 4)) +
  geom_hline(yintercept = 0, colour = "#666666") +
  geom_vline(xintercept = 0, colour = "#666666") +
  geom_point(show.legend = F)+
  labs(x = dim1, y =dim2) +
  geom_label_repel(show.legend = F) +
  guides(colour = guide_legend(override.aes = list(size = 1))) +
  scale_size(range = 2) +
  theme_estat()
rm(a2,mca1_br1,mca2_br1,mca2_br1_vars_df,cats_br1,dim1,dim2)
#ggsave("resultados/corresp_regiao.png", width = 158, height = 93, units = "mm")
