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
#pacotes e banco----
source("rdocs/source/packages.R")

banco <- read_excel('banco/análise-para-ESTAT-2.xlsx')
banco_sel<- banco[, 236:300]
banco_sel_2<- banco[, 26:50]
banco_sel_2<- banco[, 20:25]
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


##Raça----
banco_sel <- banco_sel %>%
  rename(Branca = `Dentre as opções abaixo, qual a cor ou a raça que você se identifica?`)

banco_sel <- banco_sel %>%
  mutate(Raça = case_when(
    `Branca` == 1 ~ "Branca",
    `...239` == 1 ~ "Indígena",
    `...240` == 1 ~ "Mestiça",
    `...241` == 1 ~ "Preta",
    `...242` == 1 ~ "Oriental",
    `...243` == 1 ~ "Parda",
    `...244` == 1 ~ "NULL", #Essa coluna chama-se Prefiro Não Informar (*)
    `...245` == 1 ~ "NULL"
  ))



#(*) optei por agrupar como NULL também e descartar 
banco_sel %>%
  filter(Raça == 'NULL') %>%
  count()  #7398 respostas descartadas (optaram por não responder ou preferiram não informar)


raça <- banco_sel %>%
  filter(!is.na(Raça) & Raça != "NULL") %>%  # Filtra valores NA e "NULL"
  count(Raça) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(freq, " (", n, ")") %>% str_squish()
  )


ggplot(raça) +
  aes(
    x = fct_reorder(Raça, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#CA1D1F", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "Qual a cor ou a raça que você se identifica?", 
       y = "Frequência")  +
  scale_y_continuous(breaks = seq(0, 28000, by = 7000)) +
  theme(
    panel.background = element_rect(fill = "gray90"), # Define a cor de fundo do painel
  )
ggsave("resultados/leticia/raça2.pdf", width = 158, height = 93, units = "mm")

ggsave("resultados/leticia/raça.pdf", width = 158, height = 93, units = "mm")


##Genero----
banco_sel <- banco_sel %>%
  mutate(Gênero = case_when(
    `Com qual gênero se identifica?` == 1 ~ "Mulher Cisgênero",
    `...247` == 1 ~ "Mulher Transgênero",
    `...248` == 1 ~ "Homem Cisgênero",
    `...249` == 1 ~ "Homem Transgênero",
    `...250` == 1 ~ "Não Binário",
    `...251` == 1 ~ "NULL", #Essa coluna chama-se Prefiro Não Informar (*)
    `...252` == 1 ~ "NULL") 
  )

#(*) optei por agrupar como NULL também e descartar
banco_sel %>%
  filter(Gênero == 'NULL') %>%
  count()  #11226 respostas descartadas (optaram por não responder ou preferiram não informar)

genero <- banco_sel %>%
  filter(!is.na(Gênero) & Gênero != "NULL") %>%  # Filtra valores NA e "NULL"
  count(Gênero) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )


ggplot(genero) +
  aes(
    x = fct_reorder(Gênero, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#CA1D1F", width = 0.7) +
  geom_text(
    position = position_dodge(width = ),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "Com qual gênero se identifica?", 
       y = "Frequência") +
  scale_y_continuous(breaks = c (0,5500,11000,16500,
                                 22000)) +
  scale_x_discrete(
    breaks = c("Mulher Cisgênero", "Homem Cisgênero", 
               "Não Binário", "Homem Transgênero", 
               "Mulher Transgênero"),
    labels = c("Mulher\nCisgênero", "Homem\nCisgênero",
               "Não\nBinário", "Homem\nTransgênero",
               "Mulher\nTransgênero")
  ) +
  theme(
    panel.background = element_rect(fill = "gray90"), # Define a cor de fundo do painel
  )
ggsave("resultados/leticia/genero2.pdf", width = 158, height = 93, units = "mm")

ggsave("resultados/leticia/genero.pdf", width = 158, height = 93, units = "mm")

#Renda individual-------------

renda_ind <- banco_sel_2 %>%
  mutate(Renda = case_when(
    `Qual a sua renda mensal individual, aproximadamente?` == 1 ~ "Nenhuma renda",
    `...27` == 1 ~ "Até 1 salário mínimo",
    `...28` == 1 ~ "De 1 a 3 salários mínimos",
    `...29` == 1 ~ "De 3 a 6 salários mínimos",
    `...30` == 1 ~ "De 6 a 9 salários mínimos",
    `...31` == 1 ~ "De 9 a 12 salários mínimos", 
    `...32` == 1 ~ "De 12 a 15 salários mínimos",
    `...33` == 1 ~ "Mais de 15 salários mínimos",
    `...34` == 1 ~ "NULL"))

renda_ind %>%
  filter(Renda == 'NULL') %>%#4243 descartados
  count()

renda_ind_cont <- renda_ind %>%
  filter(!is.na(Renda) & Renda != "NULL") %>%  # Filtra valores NA e "NULL"
  count(Renda) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(freq, " (", n, ")") %>% str_squish()
  )


ggplot(renda_ind_cont) +
  aes(
    x = fct_reorder(Renda, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#CA1D1F", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 2.5
  ) +
  labs(x = "Qual a sua renda mensal individual, aproximadamente?", 
       y = "Frequência") +
  scale_x_discrete(
    breaks = c("Até 1 salário mínimo",
               "De 1 a 3 salários mínimos",
               "De 3 a 6 salários mínimos",
               "De 6 a 9 salários mínimos",
               "De 9 a 12 salários mínimos", 
               "De 12 a 15 salários mínimos",
               "Mais de 15 salários mínimos",
               "Nenhuma renda"),
    labels = c("Até 1\nsalário",
               "1 a 3 \nsalários",
               "3 a 6 \nsalários",
               "6 a 9 \nsalários",
               "9 a 12 \nsalários", 
               "12 a 15 \nsalários",
               "Mais de 15 \nsalários",
               "Nenhuma \nrenda")
  ) +
  theme(
    panel.background = element_rect(fill = "gray90"), # Define a cor de fundo do painel
  )+
  scale_y_continuous(breaks=c(0,3500,7000,10500,14000))
ggsave("resultados/leticia/renda_ind2.pdf", width = 158, height = 93, units = "mm")

ggsave("resultados/leticia/renda_ind.pdf", width = 158, height = 93, units = "mm")


#Deficiencia-------
##grafico de setores def-----------------
banco_sel_4<- banco[, 253:270]

deficiencia<- banco_sel_4 %>%
  mutate(Def = case_when( 
    `Possui alguma deficiência?` == 1 ~ "Sim",
    `...254` == 1 ~ "Sim",
    `...255` == 1 ~ "Sim",
    `...256` == 1 ~ "Sim",
    `...257` == 1 ~ "Não",
    `...258` == 1 ~ "NULL"))


deficiencia%>%
  filter(Def == 'NULL') %>%
  count()  #6631 respostas descartadas 

contagem_deficiencia<- deficiencia%>%
  filter(!is.na(Def) & Def!= "NULL")%>% #retirando aqui os NAs
  group_by(Def) %>%
  summarise(Freq = n()) %>%
  mutate(Prop = round(100 * (Freq / sum(Freq)), 2)) %>%
  arrange(desc(Def)) %>%
  mutate(posicao = cumsum(Prop) - 0.5 * Prop,
         ymax = cumsum(Prop),
         ymin = c(0, head(ymax, n=-1)))%>%
  mutate(
    Freq = format(Freq, decimal.mark = ",", big.mark = ".")
  )
porcentagens <- str_c(contagem_deficiencia$Prop , "%") %>% str_replace ("\\.", ",")
label <- str_squish(str_c(porcentagens, " (", contagem_deficiencia$Freq, ")"))


ggplot(contagem_deficiencia) +
  aes(x = factor(""),
      y = Prop , 
      fill = factor(Def)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(
    aes(x = 1.8, y = c(8,59.505), label = label),
    color = "black"
  ) +
  theme_void() +
  theme(legend.position = "top") +
  theme(
    panel.background = element_rect(fill = "gray90"), # Define a cor de fundo do painel
  )+
  scale_fill_manual(values = cores_estat, name = 'Possui alguma deficiência?') +
  guides(fill = guide_legend(nrow = 1))
ggsave("resultados/leticia/setor_def.pdf", width = 158, height = 93, units = "mm")

##grafico de barras def---------------

deficiencia2<- banco_sel_4 %>%
  mutate(Def2= case_when( 
    `Possui alguma deficiência?` == 1 ~ "Física",
    `...254` == 1 ~ "Mental",
    `...255` == 1 ~ "Intelectual",
    `...256` == 1 ~ "Sensorial",
    `...257` == 1 ~ "NA", #Nenhuma
    `...258` == 1 ~ "NULL"))

deficiencia2 %>%
  filter(Def2 == 'NULL') %>%
  count()  #6631 respostas descartadas - bateu, tudo ok



contagem_deficiencia2<- deficiencia2 %>%
  filter(Def2 != "NA" & Def2 != "NULL") %>%  # Filtra valores NA e "NULL"
  count(Def2) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(freq, " (", n, ")") %>% str_squish()
  )


ggplot(contagem_deficiencia2) +
  aes(
    x = fct_reorder(Def2, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#CA1D1F", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 2.5
  ) +
  labs(x = "Deficiências", 
       y = "Frequência") +
  theme(
    panel.background = element_rect(fill = "gray90"), # Define a cor de fundo do painel
  ) + scale_y_continuous(breaks = seq(0, 13500, by = 3300))
ggsave("resultados/leticia/coluna_def.pdf", width = 158, height = 93, units = "mm")

#Rensa Familiar -------

renda_mensal <- banco_sel_2 %>%
  mutate(Mensal = case_when(
    `Somando a sua renda com a renda das pessoas que moram com você, quanto é, aproximadamente, a renda familiar mensal?` ==
      1 ~ "Nenhuma renda",
    `...36` == 1 ~ "Até 1 salário mínimo",
    `...37` == 1 ~ "De 1 a 3 salários mínimos",
    `...38` == 1 ~ "De 3 a 6 salários mínimos",
    `...39` == 1 ~ "De 6 a 9 salários mínimos",
    `...40` == 1 ~ "De 9 a 12 salários mínimos", 
    `...41` == 1 ~ "De 12 a 15 salários mínimos",
    `...42` == 1 ~ "Mais de 15 salários mínimos",
    `...43` == 1 ~ "NULL"))

renda_mensal %>%
  filter(Mensal == 'NULL') %>%#5270 descartados
  count()

renda_mensal_cont <- renda_mensal %>%
  filter(!is.na(Mensal) & Mensal != "NULL") %>%  # Filtra valores NA e "NULL"
  count(Mensal) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(freq, " (", n, ")") %>% str_squish()
  )


ggplot(renda_mensal_cont) +
  aes(
    x = fct_reorder(Mensal, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#CA1D1F", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9 ),
    vjust = -0.5, # hjust = .5,
    size = 2.3
  ) +
  labs(x = "Somando a sua renda com a renda das pessoas que moram \ncom você, quanto é, aproximadamente, a renda familiar mensal?", 
       y = "Frequência") +
  scale_x_discrete(
    breaks = c("Até 1 salário mínimo",
               "De 1 a 3 salários mínimos",
               "De 3 a 6 salários mínimos",
               "De 6 a 9 salários mínimos",
               "De 9 a 12 salários mínimos", 
               "De 12 a 15 salários mínimos",
               "Mais de 15 salários mínimos",
               "Nenhuma renda"),
    labels = c("Até 1\nsalário",
               "1 a 3 \nsalários",
               "3 a 6 \nsalários",
               "6 a 9 \nsalários",
               "9 a 12 \nsalários", 
               "12 a 15 \nsalários",
               "Mais de 15 \nsalários",
               "Nenhuma \nrenda"))  +
  theme(panel.background = element_rect(fill = "gray90"))+ # Define a cor de fundo do painel 
  scale_y_continuous(breaks = seq(0, 12500, by = 2800))

ggsave("resultados/leticia/renda_mensal2.pdf", width = 158, height = 93, units = "mm")
ggsave("resultados/leticia/renda_mensal.pdf", width = 158, height = 93, units = "mm")

#Número de dependentes financeiros-----
##grafico de setores sim ou não-----------------

banco_sel_3<- banco[, 339:370]

dependentes <- banco_sel_3 %>%
  mutate(Dependentes = case_when( 
    `Possui dependentes financeiros (Selecionar quantos necessários)?` == "1" ~ "Sim",
    `...340` == 1 ~ "Sim",
    `...341` == 1 ~ "Sim",
    `...343` == 1 ~ "Sim",
    `...342` == 1 ~ "Não", 
    `...344` == 1 ~ "NULL"))


dependentes %>%
  filter(Dependentes == 'NULL') %>%
  count()  #7281 respostas descartadas 

contagem_dependentes <- dependentes %>%
  filter(!is.na(Dependentes) & Dependentes!= "NULL")%>% #retirando aqui os NAs
  group_by(Dependentes) %>%
  summarise(Freq = n()) %>%
  mutate(Prop = round(100 * (Freq / sum(Freq)), 2)) %>%
  arrange(desc(Dependentes)) %>%
  mutate(posicao = cumsum(Prop) - 0.7 * Prop,
         ymax = cumsum(Prop),
         ymin = c(0, head(ymax, n=-1)))%>%
  mutate(
    Freq = format(Freq, decimal.mark = ",", big.mark = ".")
  )
porcentagens <- str_c(contagem_dependentes$Prop , "%") %>% str_replace ("\\.", ",")
label <- str_squish(str_c(porcentagens, " (", contagem_dependentes$Freq, ")"))


ggplot(contagem_dependentes) +
  aes(x = factor(""),
      y = Prop , 
      fill = factor(Dependentes)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(
    aes(x = 1.8, y = posicao, label = label),
    color = "black",  position = position_stack(vjust = 0.6)
  ) +
  theme_void() +
  theme(legend.position = "top") +
  theme(
    panel.background = element_rect(fill = "gray90"), # Define a cor de fundo do painel
  )+
  scale_fill_manual(values = cores_estat, name = 'Possui dependentes financeiros?') +
  guides(fill = guide_legend(nrow = 1))
ggsave("resultados/leticia/dependentes.pdf", width = 158, height = 93, units = "mm")

##grafico de barras (quais dependentes)---------------

banco_sel_3<- banco[, 339:370]

dependentes2 <- banco_sel_3 %>%
  mutate(Dependentes2 = case_when( 
    `Possui dependentes financeiros (Selecionar quantos necessários)?` 
    == "1" ~ "Cônjuge e ex-cônjuges",
    `...340` == 1 ~ "Filhos",
    `...341` == 1 ~ "Idosos",
    `...343` == 1 ~ "Outros",
    `...342` == 1 ~ "NA", #Não possuo dependentes
    `...344` == 1 ~ "NULL"))



dependentes2_cont <- dependentes2 %>%
  filter(Dependentes2 != "NA" & Dependentes2 != "NULL") %>%  # Filtra valores NA e "NULL"
  count(Dependentes2) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(freq, " (", n, ")") %>% str_squish()
  )


ggplot(dependentes2_cont) +
  aes(
    x = fct_reorder(Dependentes2, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#CA1D1F", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 2.5
  ) +
  labs(x = "Dependentes financeiros", 
       y = "Frequência") +
  scale_x_discrete(
    breaks = c("Cônjuge e ex-cônjuges",
               "Filhos",
               "Idosos",
               "Não possuo dependentes",
               "Outros"),
    labels = c("Cônjuge e \nex-cônjuges",
               "Filhos",
               "Idosos",
               "Não possuo \ndependentes",
               "Outros")
  ) +
  theme(
    panel.background = element_rect(fill = "gray90"), # Define a cor de fundo do painel
  ) + scale_y_continuous(breaks = seq(0, 9000, by = 2250))
ggsave("resultados/leticia/dependentes2.pdf", width = 158, height = 93, units = "mm")


#Contriuição para previdencia------------
##grafico de setores prev-----------------
banco_sel_3<- banco[, 339:370]

previdencia <- banco_sel_3 %>%
  mutate(Prev = case_when( 
    `...352` == 1 ~ "Sim",
    `...353` == 1 ~ "Sim",
    `...354` == 1 ~ "Sim",
    `...355` == 1 ~ "Não",
    `...356` == 1 ~ "NULL"))


previdencia %>%
  filter(Prev == 'NULL') %>%
  count()  #7549 respostas descartadas 

contagem_previdencia <- previdencia %>%
  filter(!is.na(Prev) & Prev!= "NULL")%>% #retirando aqui os NAs
  group_by(Prev) %>%
  summarise(Freq = n()) %>%
  mutate(Prop = round(100 * (Freq / sum(Freq)), 2)) %>%
  arrange(desc(Prev)) %>%
  mutate(posicao = cumsum(Prop) - 0.5 * Prop,
         ymax = cumsum(Prop),
         ymin = c(0, head(ymax, n=-1)))%>%
  mutate(
    Freq = format(Freq, decimal.mark = ",", big.mark = ".")
  )
porcentagens <- str_c(contagem_previdencia$Prop , "%") %>% str_replace ("\\.", ",")
label <- str_squish(str_c(porcentagens, " (", contagem_previdencia$Freq, ")"))


ggplot(contagem_previdencia) +
  aes(x = factor(""),
      y = Prop , 
      fill = factor(Prev)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(
    aes(x = 1.8, y = c(8,59.505), label = label),
    color = "black"
  ) +
  theme_void() +
  theme(legend.position = "top") +
  theme(
    panel.background = element_rect(fill = "gray90"), # Define a cor de fundo do painel
  )+
  scale_fill_manual(values = cores_estat, name = 'Contribui para a previdência?') +
  guides(fill = guide_legend(nrow = 1))
ggsave("resultados/leticia/setor_prev.pdf", width = 158, height = 93, units = "mm")

##grafico de barras prev---------------

previdencia2 <- banco_sel_3 %>%
  mutate(Prev2 = case_when( 
    `...352` == 1 ~ "Privada",
    `...353` == 1 ~ "Pública",
    `...354` == 1 ~ "Pública/Privada",
    `...355` == 1 ~ "NA", #Não
    `...356` == 1 ~ "NULL"))

previdencia2 %>%
  filter(Prev2 == 'NULL') %>%
  count()  #7549 respostas descartadas - bateu, tudo ok



contagem_previdencia2<- previdencia2 %>%
  filter(Prev2 != "NA" & Prev2 != "NULL") %>%  # Filtra valores NA e "NULL"
  count(Prev2) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(freq, " (", n, ")") %>% str_squish()
  )


ggplot(contagem_previdencia2) +
  aes(
    x = fct_reorder(Prev2, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#CA1D1F", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 2.5
  ) +
  labs(x = "Previdência", 
       y = "Frequência") +
  theme(
    panel.background = element_rect(fill = "gray90"), # Define a cor de fundo do painel
  ) + scale_y_continuous(breaks = seq(0, 13500, by = 3300))
ggsave("resultados/leticia/coluna_prev.pdf", width = 158, height = 93, units = "mm")

##Imovel ---------------

teste <- banco_sel_2 %>%
  mutate(Imóvel = case_when( 
    `Possui imóvel próprio?` == "1" ~ "Sim",
    `...21` == 1 ~ "Não", 
    `...22` == 1 ~ "NULL"))

teste %>%
  filter(Imóvel == 'NULL') %>%
  count()  #3288 respostas descartadas 

contagem_imovel <- teste %>%
  filter(!is.na(Imóvel) & Imóvel!= "NULL")%>% #retirando aqui os NAs
  group_by(Imóvel) %>%
  summarise(Freq = n()) %>%
  mutate(Prop = round(100 * (Freq / sum(Freq)), 2)) %>%
  arrange(desc(Imóvel)) %>%
  mutate(posicao = cumsum(Prop) - 0.7 * Prop,
         ymax = cumsum(Prop),
         ymin = c(0, head(ymax, n=-1)))%>%
  mutate(
    Freq = format(Freq, decimal.mark = ",", big.mark = ".")
  )
porcentagens <- str_c(contagem_imovel$Prop , "%") %>% str_replace ("\\.", ",")
label <- str_squish(str_c(porcentagens, " (", contagem_imovel$Freq, ")"))


ggplot(contagem_imovel) +
  aes(x = factor(""),
      y = Prop , 
      fill = factor(Imóvel)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(
    aes(x = 1.8, y = posicao, label = label),
    color = "black",  position = position_stack(vjust = 0.6)
  ) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "gray90"), # Define a cor de fundo do painel
  )+
  theme(legend.position = "top") +
  scale_fill_manual(values = cores_estat, name = 'Possui imóvel próprio?') +
  guides(fill = guide_legend(nrow = 1)) +
  
ggsave("resultados/leticia/imovel.pdf", width = 158, height = 93, units = "mm")


#testando agora retirando as duas primerias linhas
#teste <- teste[c(-1:-2), 31]
#obtive os mesmos resultados (por conta da segunda linha no código de contagem)






#Carro------------

#fazendo um teste para ver se preciso descartar o primeiro Sim 
#banco_sel_2 %>%
#  filter(`Possui imóvel próprio?` == '1') %>%
#  count() 
#resultado=18395 - certo 

carro <- banco_sel_2 %>%
  mutate(Carro = case_when( 
    `Possui carro próprio?` == "1" ~ "Sim",
    `...24` == "1" ~ "Não", 
    `...25` == "1" ~ "NULL"))


carro %>%
  filter(Carro == 'NULL') %>%
  count()  #3381 respostas descartadas 

contagem_carro <- carro %>%
  filter(!is.na(Carro) & Carro!= "NULL")%>% #retirando aqui os NAs
  group_by(Carro) %>%
  summarise(Freq = n()) %>%
  mutate(Prop = round(100 * (Freq / sum(Freq)), 2)) %>%
  mutate(posicao = cumsum(Prop) - 0.7 * Prop,
         ymax = cumsum(Prop),
         ymin = c(0, head(ymax, n=-1)))%>%
  mutate(
    Freq = format(Freq, decimal.mark = ",", big.mark = ".")
  )
porcentagens <- str_c(contagem_carro$Prop , "%") %>% str_replace ("\\.", ",")
label <- str_squish(str_c(porcentagens, " (", contagem_carro$Freq, ")"))

ggplot(contagem_carro) +
  aes(x = factor(""),
      y = Prop , 
      fill = factor(Carro)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(
    aes(x = 1.8, y =c(20, 78), label = label),
    color = "black",  position = position_stack(vjust = 0.63)
  ) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "gray90"), # Define a cor de fundo do painel
  )+
  theme(legend.position = "top") +
  scale_fill_manual(values = cores_estat, name = 'Possui carro próprio?') +
  guides(fill = guide_legend(nrow = 1))
ggsave("resultados/leticia/carro.pdf", 
       width = 158, height = 93, units = "mm")





#Fonte de renda


banco_sel_3<- banco[, 339:370]

dependentes2 <- banco_sel_3 %>%
  mutate(Dependentes2 = case_when( 
    `Possui dependentes financeiros (Selecionar quantos necessários)?` 
    == "1" ~ "Cônjuge e ex-cônjuges",
    `...340` == 1 ~ "Filhos",
    `...341` == 1 ~ "Idosos",
    `...343` == 1 ~ "Outros",
    `...342` == 1 ~ "NA", #Não possuo dependentes
    `...344` == 1 ~ "NULL"))



dependentes2_cont <- dependentes2 %>%
  filter(Dependentes2 != "NA" & Dependentes2 != "NULL") %>%  # Filtra valores NA e "NULL"
  count(Dependentes2) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(freq, " (", n, ")") %>% str_squish()
  )


ggplot(dependentes2_cont) +
  aes(
    x = fct_reorder(Dependentes2, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#CA1D1F", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 2.5
  ) +
  labs(x = "Dependentes financeiros", 
       y = "Frequência") +
  scale_x_discrete(
    breaks = c("Cônjuge e ex-cônjuges",
               "Filhos",
               "Idosos",
               "Não possuo dependentes",
               "Outros"),
    labels = c("Cônjuge e \nex-cônjuges",
               "Filhos",
               "Idosos",
               "Não possuo \ndependentes",
               "Outros")
  ) +
  theme(
    panel.background = element_rect(fill = "gray90"), # Define a cor de fundo do painel
  )
ggsave("resultados/leticia/dependentes2.pdf", width = 158, height = 93, units = "mm")


#Fonte de renda

banco_sel_6<- banco[, 7:19]

fonte <- banco_sel_6 %>%
  mutate(Fonte = case_when( 
    `Acerca de sua(s) fonte(s) de renda?` 
    == 1 ~ "Aposentado ou Pensionistas",
    `...8` == 1 ~ "Assalariado 1",
    `...9` == 1 ~ "Assalariado 2",
    `...10` == 1 ~ "Assalariado 3",
    `...11` == 1 ~ "Assalariado 4",
    `...12` == 1 ~ "Autônomo 1",
    `...13` == 1 ~ "Autônomo 2",
    `...14` == 1 ~ "Empresário 1",
    `...15` == 1 ~ "Empresário 2",
    `...16` == 1 ~ "Outras fontes",
    `...17` == 1 ~ "Renda Proveniente de Aluguel",
    `...18` == 1 ~ "Seguro Desemprego",
    `...19` == 1 ~ "NULL"))


fonte %>%
  filter(Fonte == 'NULL') %>%
  count()  #3488 respostas descartadas 


fonte_cont <- fonte %>%
  filter(Fonte != "NA" & Fonte != "NULL") %>%  # Filtra valores NA e "NULL"
  count(Fonte) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(freq, " (", n, ")") %>% str_squish()
  )


#esse gráfico foi descartado
ggplot(fonte_cont) +
  aes(
    x = fct_reorder(Fonte, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#CA1D1F", width = 0.7) +
  geom_text(
    position = position_dodge(width = 0.2),
    vjust = -0.2, hjust = -0.1,
    size = 2.3
  ) +
  labs(x = "Fonte de renda", 
       y = "Frequência") +
  scale_x_discrete(
    breaks = c("Aposentado ou Pensionistas",
               "Assalariado 1",
               "Assalariado 2",
               "Assalariado 3",
               "Assalariado 4",
               "Autônomo 1",
               "Autônomo 2",
               "Empresário 1",
               "Empresário 2",
               "Outras fontes",
               "Renda Proveniente de Aluguel",
               "Seguro Desemprego"),
    labels = c("Aposentado ou \nPensionistas",
               "Assalariado 1",
               "Assalariado 2",
               "Assalariado 3",
               "Assalariado 4",
               "Autônomo 1",
               "Autônomo 2",
               "Empresário 1",
               "Empresário 2",
               "Outras fontes",
               "Renda Proveniente de Aluguel",
               "Seguro Desemprego")
  ) +
  theme(plot.margin = margin(r = 1),
    panel.background = element_rect(fill = "gray90"), # Define a cor de fundo do painel
  ) +
  scale_y_continuous(breaks=c(0,4500,9000,13500,18000)) + 
  coord_flip()





ggplot(fonte_cont) +
  aes(
    x = fct_reorder(Fonte, n, .desc = T),
    y = n
  ) +
  geom_bar(stat = "identity", fill = "#CA1D1F", width = 0.8) +
  geom_text(
    aes(label = freq),
    vjust = -0.2, hjust = -.2,
    size = 2.2
  ) +
  geom_text(
    aes(label = str_c(" (", n, ")")),
    vjust = 1.2, hjust = -.2,
    size = 2.2
  ) +
  labs(x = "Fonte de renda", 
       y = "Frequência") +
  scale_x_discrete(
    breaks = c("Aposentado ou Pensionistas",
               "Assalariado 1",
               "Assalariado 2",
               "Assalariado 3",
               "Assalariado 4",
               "Autônomo 1",
               "Autônomo 2",
               "Empresário 1",
               "Empresário 2",
               "Outras fontes",
               "Renda Proveniente de Aluguel",
               "Seguro Desemprego"),
    labels = c("Aposentado ou \nPensionistas",
               "Assalariado 1",
               "Assalariado 2",
               "Assalariado 3",
               "Assalariado 4",
               "Autônomo 1",
               "Autônomo 2",
               "Empresário 1",
               "Empresário 2",
               "Outras fontes",
               "Proveniente \nde Aluguel",
               "Seguro \n Desemprego")
  ) +
  theme(plot.margin = margin(r = 1),
        panel.background = element_rect(fill = "gray90"), # Define a cor de fundo do painel
  ) +
  scale_y_continuous(
    breaks = seq(0, 20000, 4500),
    limits = c(0, 20000)
  ) + 
  coord_flip() +
  expand_limits(y = 20000)

ggsave("resultados/leticia/fonte.pdf", width = 158, height = 93, units = "mm")
