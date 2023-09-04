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
  mutate(freq_relativa = freq %>%
           percent())

porcentagens <- str_c(banco1$freq_relativa , "%") %>% str_replace ("\\.", ",")
legendas <- str_squish(str_c(banco1$freq, " (", porcentagens, ")"))
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
  mutate(freq_relativa = freq %>%
           percent())

porcentagens <- str_c(banco2$freq_relativa , "%") %>% str_replace ("\\.", ",")
legendas <- str_squish(str_c(banco2$freq, " (", porcentagens, ")"))

ggplot(banco2) +
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
  mutate(freq_relativa = freq %>%
           percent())

porcentagens <- str_c(banco3$freq_relativa , "%") %>% str_replace ("\\.", ",")
legendas <- str_squish(str_c(banco3$freq, " (", porcentagens, ")"))

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

