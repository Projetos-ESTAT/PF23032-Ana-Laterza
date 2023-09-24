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

banco <- read_excel('banco/análise-para-ESTAT-2.xlsx', sheet = 1)

# “Qual o seu nível de satisfação com a instituição de ensino” ----

banco1 <- banco[, 49:54]
banco1 <- banco1 %>%
  mutate(Resposta = case_when(
    `Qual o seu nível de satisfação com a Instituição de Ensino onde concluiu sua formação como arquiteto e urbanista?` == 1 ~ "Totalmente satisfeito
",
    ...50 == 1 ~ "Parcialmente satisfeito",
    ...51 == 1 ~ "Indiferente",
    ...52 == 1 ~ "Parcialmente Insatisfeito",
    ...53 == 1 ~ "Totalmente insatisfeito",
    ...54 == 1 ~ "NULL"
  ))

banco1 <- banco1[c(-1:-2), 7]

banco1 %>%
  filter(Resposta == 'NULL') %>%
  count() # 4147 respostas nulas

banco1 <- banco1 %>%
  filter(Resposta != 'NULL') %>%
  group_by(Resposta) %>%
  summarise(freq = n()) %>%
  mutate(freq1 = freq) %>%
  mutate(freq_relativa = freq %>%
           percent()) %>%
  mutate(across(freq1, ~ format(., big.mark = ".", scientific = F)))

porcentagens <- str_c(banco1$freq_relativa , "%") %>% str_replace ("\\.", ",")
legendas <- str_squish(str_c(banco1$freq1, " (", porcentagens, ")"))
banco1$Resposta <- str_to_title(banco1$Resposta)
banco1$Resposta <- trimws(banco1$Resposta)

ordem <- c("Totalmente Insatisfeito", "Parcialmente Insatisfeito",
           "Indiferente", "Parcialmente Satisfeito",
           "Totalmente Satisfeito")

banco1$Resposta <- factor(banco1$Resposta, levels = ordem) 

ggplot(banco1) +
  aes(
    x = Resposta,
    y = freq,
    label = legendas
  ) +
  geom_bar(stat = "identity", fill = "#CA1D1F", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 2.85
  ) +
  labs(x = "Qual o seu nível de satisfação com a Instituição de Ensino\nonde concluiu sua formação como arquiteto e urbanista?", y = "Frequência") +
  scale_x_discrete(labels = wrap_format(15)) +
  theme_estat()
ggsave("resultados/analu/nivel-satisfacao-IES.pdf", width = 158, height = 93, units = "mm")

# “Você atua como docente na área…” ----

banco2 <- banco[, 425:428]
banco2 <- banco2 %>%
  mutate(Resposta = case_when(
    `Você atua como docente na área de arquitetura e urbanismo?` == 1 ~ "Sim com dedicação exclusiva",
  ...426 == 1 ~ "Sim e também como profissional",
  ...427 == 1 ~ "Não",
  ...428 == 1 ~ "NULL"))

banco2 <- banco2[c(-1:-2), 5]

banco2 %>%
  filter(Resposta == 'NULL') %>%
  count() # 8091 respostas nulas

banco2 <- banco2 %>%
  filter(Resposta != 'NULL') %>%
  group_by(Resposta) %>%
  summarise(freq = n()) %>%
  mutate(freq1 = freq) %>%
  mutate(freq_relativa = freq %>%
           percent()) %>%
  mutate(across(freq1, ~ format(., big.mark = ".", scientific = F)))

porcentagens <- str_c(banco2$freq_relativa , "%") %>% str_replace ("\\.", ",")
legendas <- str_squish(str_c(banco2$freq1, " (", porcentagens, ")"))

ggplot(banco2) +
  aes(
    x = fct_reorder(Resposta, freq, .desc = T),
    y = freq,
    label = legendas
  ) +
  geom_bar(stat = "identity", fill = "#CA1D1F", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "Você atua como docente na área de arquitetura e urbanismo?", y = "Frequência") +
  scale_x_discrete(labels = wrap_format(20)) +
  theme_estat()
ggsave("resultados/analu/docente.pdf", width = 158, height = 93, units = "mm")

# “Além de arquitetura…” (se tem outro curso superior) ----

banco3 <- banco[, 364:366]
banco3 <- banco3 %>%
  mutate(Resposta = case_when(
    `Além de arquitetura e urbanismo possui outro curso superior?` == 1 ~ "Sim",
    ...365 == 1 ~ "Não",
    ...366 == 1 ~ "NULL"))

banco3 <- banco3[c(-1:-2), 4]

banco3 %>%
  filter(Resposta == 'NULL') %>%
  count() # 7142 respostas nulas

banco3 <- banco3 %>%
  filter(Resposta != 'NULL') %>%
  group_by(Resposta) %>%
  summarise(freq = n()) %>%
  mutate(freq1 = freq) %>%
  mutate(freq_relativa = freq %>%
           percent()) %>%
  mutate(across(freq1, ~ format(., big.mark = ".", scientific = F)))

porcentagens <- str_c(banco3$freq_relativa , "%") %>% str_replace ("\\.", ",")
legendas <- str_squish(str_c(banco3$freq1, " (", porcentagens, ")"))

ggplot(banco3) +
  aes(
    x = Resposta,
    y = freq,
    label = legendas
  ) +
  geom_bar(stat = "identity", fill = "#CA1D1F", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "Além de Arquitetura e Urbanismo possui\noutro curso superior?", y = "Frequência") +
  scale_x_discrete(labels = wrap_format(20)) +
  theme_estat()
ggsave("resultados/analu/outro-curso-superior.pdf", width = 158, height = 93, units = "mm")

# Se está cursando outro curso superior ----

banco4 <- banco[, 367:369]
banco4 <- banco4 %>%
  mutate(Resposta = case_when(
    `Está cursando algum outro curso superior?` == 1 ~ "Sim",
    ...368 == 1 ~ "Não",
    ...369 == 1 ~ "NULL"))

banco4 <- banco4[c(-1:-2), 4]

banco4 %>%
  filter(Resposta == 'NULL') %>%
  count() #7202 respostas nulas

banco4 <- banco4 %>%
  filter(Resposta != 'NULL') %>%
  group_by(Resposta) %>%
  summarise(freq = n()) %>%
  mutate(freq1 = freq) %>%
  mutate(freq_relativa = freq %>%
           percent()) %>%
  mutate(across(freq1, ~ format(., big.mark = ".", scientific = F)))

porcentagens <- str_c(banco4$freq_relativa , "%") %>% str_replace ("\\.", ",")
legendas <- str_squish(str_c(banco4$freq1, " (", porcentagens, ")"))

ggplot(banco4) +
  aes(
    x = Resposta,
    y = freq,
    label = legendas
  ) +
  geom_bar(stat = "identity", fill = "#CA1D1F", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "Está cursando algum outro curso superior?", y = "Frequência") +
  scale_y_continuous(breaks = seq(0, 40000, by = 5000), limits = c(0, 40000)) +
  theme_estat()
ggsave("resultados/analu/cursando-outro-curso-superior.pdf", width = 158, height = 93, units = "mm")

# Pretende fazer outro curso superior ----

banco5 <- banco[, 370:372]
banco5 <- banco5 %>%
  mutate(Resposta = case_when(
    `Pretende fazer outro curso superior?` == 1 ~ "Sim",
    ...371 == 1 ~ "Não",
    ...372 == 1 ~ "NULL"))

banco5 <- banco5[c(-1:-2), 4]

banco5 %>%
  filter(Resposta == 'NULL') %>%
  count() #7262 respostas nulas

banco5 <- banco5 %>%
  filter(Resposta != 'NULL') %>%
  group_by(Resposta) %>%
  summarise(freq = n()) %>%
  mutate(freq1 = freq) %>%
  mutate(freq_relativa = freq %>%
           percent()) %>%
  mutate(across(freq1, ~ format(., big.mark = ".", scientific = F)))

porcentagens <- str_c(banco5$freq_relativa , "%") %>% str_replace ("\\.", ",")
legendas <- str_squish(str_c(banco5$freq1, " (", porcentagens, ")"))

ggplot(banco5) +
  aes(
    x = Resposta,
    y = freq,
    label = legendas
  ) +
  geom_bar(stat = "identity", fill = "#CA1D1F", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "Pretende fazer outro curso superior?", y = "Frequência") +
  scale_y_continuous(breaks = seq(0, 40000, by = 5000), limits = c(0, 40000)) +
  theme_estat()
ggsave("resultados/analu/pretende-fazer-outro-curso-superior.pdf", width = 158, height = 93, units = "mm")

# Participação em feiras

banco6 <- banco[, 44:48]
banco6 <- banco6 %>%
  mutate(Resposta = case_when(
    `Como é a sua participação em feiras, seminários, congressos e afins na área da arquitetura e urbanismo?` == 1 ~ "Não frequenta",
    ...45 == 1 ~ "Frequenta algumas",
    ...46 == 1 ~ "Frequenta a maioria",
    ...47 == 1 ~ "Frequenta todas",
    ...48 == 1 ~ "NULL"
    )
    )

banco6 <- banco6[c(-1:-2), 6]

banco6 %>%
  filter(Resposta == 'NULL') %>%
  count() #4041 respostas nulas

banco6 <- banco6 %>%
  filter(Resposta != 'NULL') %>%
  group_by(Resposta) %>%
  summarise(freq = n()) %>%
  mutate(freq1 = freq) %>%
  mutate(freq_relativa = freq %>%
           percent()) %>%
  mutate(across(freq1, ~ format(., big.mark = ".", scientific = F)))

porcentagens <- str_c(banco6$freq_relativa , "%") %>% str_replace ("\\.", ",")
legendas <- str_squish(str_c(banco6$freq1, " (", porcentagens, ")"))


ordem <- c("Não frequenta", "Frequenta algumas",
           "Frequenta a maioria", "Frequenta todas")

banco6$Resposta <- factor(banco6$Resposta, levels = ordem) 

ggplot(banco6) +
  aes(
    x = Resposta,
    y = freq,
    label = legendas
  ) +
  geom_bar(stat = "identity", fill = "#CA1D1F", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "Como é a sua participação em feiras, seminários, congressos e afins\nna área da arquitetura e urbanismo?", y = "Frequência") +
  scale_y_continuous(breaks = seq(0, 30000, by = 5000), limits = c(0, 30000)) +
  theme_estat()
ggsave("resultados/analu/frequenta-feira.pdf", width = 158, height = 93, units = "mm")

# Marque abaixo a opção…” (grau de escolaridade) ----

banco7 <- banco[, 357:363]
banco7 <- banco7 %>%
  mutate(Resposta = case_when(
    `Marque abaixo a opção de acordo com seu grau de escolaridade.` == 1 ~ "Graduação",
    ...358 == 1 ~ "Pós-Graduação",
    ...360 == 1 ~ "PHD",
    ...359 == 1 ~ "Mestrado",
    ...361 == 1 ~ "Pós-PHD",
    ...362 == 1 ~ "Prefiro não informar",
    ...363 == 1 ~ "NULL"
  )
  )

banco7 <- banco7[c(-1:-2), 8]

banco7 %>%
  filter(Resposta == 'NULL') %>%
  count() #7072 respostas nulas

banco7 <- banco7 %>%
  filter(Resposta != 'NULL') %>%
  group_by(Resposta) %>%
  summarise(freq = n()) %>%
  mutate(freq1 = freq) %>%
  mutate(freq_relativa = freq %>%
           percent()) %>%
  mutate(across(freq1, ~ format(., big.mark = ".", scientific = F)))

porcentagens <- str_c(banco7$freq_relativa , "%") %>% str_replace ("\\.", ",")
legendas <- str_squish(str_c(banco7$freq1, " (", porcentagens, ")"))


ordem <- c("Graduação", "Pós-Graduação",
           "Mestrado", "PHD", "Pós-PHD", "Prefiro não informar")

banco7$Resposta <- factor(banco7$Resposta, levels = ordem) 

ggplot(banco7) +
  aes(
    x = Resposta,
    y = freq,
    label = legendas
  ) +
  geom_bar(stat = "identity", fill = "#CA1D1F", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "Grau de escolaridade", y = "Frequência") +
  scale_y_continuous(breaks = seq(0, 30000, by = 5000), limits = c(0, 30000)) +
  scale_x_discrete(labels = wrap_format(15)) +
  theme_estat()
ggsave("resultados/analu/grau-escolaridade.pdf", width = 158, height = 93, units = "mm")

# “Como você classifica o seu conhecimento em informática?” ----

banco8 <- banco[, 163:168]
banco8 <- banco8 %>%
  mutate(Resposta = case_when(
    `Como você classifica o seu conhecimento de Informática?` == 1 ~ "Muito bom",
    ...164 == 1 ~ "Bom",
    ...165 == 1 ~ "Não conheço",
    ...166 == 1 ~ "Ruim",
    ...167 == 1 ~ "Muito Ruim",
    ...168 == 1 ~ "NULL"
  )
  )

banco8 <- banco8[c(-1:-2), 7]

banco8 %>%
  filter(Resposta == 'NULL') %>%
  count() #4882 respostas nulas

banco8 <- banco8 %>%
  filter(Resposta != 'NULL') %>%
  group_by(Resposta) %>%
  summarise(freq = n()) %>%
  mutate(freq1 = freq) %>%
  mutate(freq_relativa = freq %>%
           percent()) %>%
  mutate(across(freq1, ~ format(., big.mark = ".", scientific = F)))

porcentagens <- str_c(banco8$freq_relativa , "%") %>% str_replace ("\\.", ",")
legendas <- str_squish(str_c(banco8$freq1, " (", porcentagens, ")"))


ordem <- c("Muito Ruim", "Ruim",
           "Não conheço", "Bom", "Muito bom")

banco8$Resposta <- factor(banco8$Resposta, levels = ordem) 

ggplot(banco8) +
  aes(
    x = Resposta,
    y = freq,
    label = legendas
  ) +
  geom_bar(stat = "identity", fill = "#CA1D1F", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "Grau de escolaridade", y = "Frequência") +
  scale_y_continuous(breaks = seq(0, 30000, by = 5000), limits = c(0, 30000)) +
  scale_x_discrete(labels = wrap_format(15)) +
  theme_estat()
ggsave("resultados/analu/conhecimento-informatica.pdf", width = 158, height = 93, units = "mm")

# “Com relação ao domínio de idiomas…” ----

banco9 <- banco[, 220:236]
banco9 <- banco9 %>%
  mutate(Resposta_ingles = case_when(
    `Com relação ao domínio de idiomas estrangeiros, como você considera os seus conhecimentos em:` == 1 ~ "Inglês - Básico",
    ...221 == 1 ~ "Inglês - Intermediário",
    ...222 == 1 ~ "Inglês - Avançado",
    ...223 == 1 ~ "Inglês - Fluente",
    ...224 == 1 ~ "Inglês - NULL"
  )
  ) %>%
  mutate(Resposta_frances = case_when(
    ...225 == 1 ~ "Francês - Básico",
    ...226 == 1 ~ "Francês - Intermediário",
    ...227 == 1 ~ "Francês - Avançado",
    ...228 == 1 ~ "Francês - Fluente",
    ...229 == 1 ~ "Francês - NULL"
  )
  ) %>%
  mutate(Resposta_espanhol = case_when(
    ...230 == 1 ~ "Espanhol - Básico",
    ...231 == 1 ~ "Espanhol - Intermediário",
    ...232 == 1 ~ "Espanhol - Avançado",
    ...233 == 1 ~ "Espanhol - Fluente",
    ...234 == 1 ~ "Espanhol - NULL"
  )
  ) %>%
  mutate(Outros_idiomas = case_when(
    ...235 == 1 ~ "Outras línguas",
    ...235 == 0 ~ "NULL"
  )
  ) %>%
  mutate(nao_fala = case_when(
    ...236 == 1 ~ "Não falo nenhum idioma\nestrangeiro",
    ...236 == 0 ~ "NULL"
  )
  ) 
  
banco9 <- banco9[c(-1,-2), c(18:22)]

banco9 %>%
  filter(Resposta_ingles == 'Inglês - NULL') %>%
  count() #15655 respostas nulas

banco9 %>%
  filter(Resposta_frances == 'Francês - NULL') %>%
  count() #38700 respostas nulas

banco9 %>%
  filter(Resposta_espanhol == 'Espanhol - NULL') %>%
  count() #26313 respostas nulas

x1 <- banco9[, 1]
colnames(x1) <- "idioma"

x2 <- banco9[, 2]
colnames(x2) <- "idioma"

x3 <- banco9[, 3]
colnames(x3) <- "idioma"

x4 <- banco9[, 4]
colnames(x4) <- "idioma"

x5 <- banco9[, 5]
colnames(x5) <- "idioma"

x <- rbind(x1, x2)
xx <- rbind(x, x3)
x <- rbind(xx, x4)
xx <- rbind(x, x5)

xx <- xx %>%
  mutate(idiomas = sub(" - .*", "", idioma)) %>%
  mutate(Proficiência = sub(".* - ", "", idioma)) %>%
  filter(Proficiência != 'NULL')

xx <- xx[ , -1]

xx1 <- xx %>%
  na.omit() %>%
  group_by(idiomas, Proficiência) %>%
  summarise(freq = n()) %>%
  mutate(freq1 = freq) %>%
  mutate(freq_relativa = freq %>%
           percent()) %>%
  mutate(across(freq1, ~ format(., big.mark = ".", scientific = F)))

porcentagens <- str_c(xx1$freq_relativa , "%") %>% str_replace ("\\.", ",")
legendas <- str_squish(str_c(xx1$freq1, " (", porcentagens, ")"))

ordem <- c("Básico", "Intermediário", "Avançado", "Fluente", "Não falo nenhum idioma\nestrangeiro", "Outras línguas")

ggplot(xx1, aes(x = idiomas, y = freq, fill = factor(Proficiência, levels = ordem), label = legendas)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(name = "Idioma") +
  labs(x = "Idioma", y = "Frequência relativa") +
  geom_text(position = position_fill(vjust = 0.5), size = 2.5, colour = "white") +
  scale_x_discrete(labels = wrap_format(20)) +
  theme_estat()
ggsave("resultados/analu/idiomas.pdf", width = 158, height = 93, units = "mm")

