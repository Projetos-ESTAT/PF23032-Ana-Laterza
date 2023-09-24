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
library(dplyr)





cores_personalizadas <- c("#CA1D1F", "#F55D1C", "#F55751", "#086C75") 
theme_ble <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 10),
      axis.title.x = ggplot2::element_text(colour = "black", size = 10),
      axis.text = ggplot2::element_text(colour = "black", size = 10),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      text = element_text(family = "sans", size = 12),
      legend.position = "top",
      ...
    )
  return(
    list(
      theme,
      scale_fill_manual(values = cores_personalizadas),
      scale_colour_manual(values = cores_personalizadas),
      scale_y_continuous(
        labels = scales::number_format(decimal.mark = ',',
                                       #                                       accuracy = 0.01,
                                       big.mark = "."))
    )
  )
}






#sites conselho

banco <- banco %>%
  mutate(across(everything(), ~ ifelse(. == "0.0", NA, .)))

siteconse <- banco %>%
  filter(!is.na(`Você frequenta os sites do Conselho de Arquitetura e Urbanismo?`)) %>%
  count(`Você frequenta os sites do Conselho de Arquitetura e Urbanismo?`) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )


siteconse <- banco %>%
  filter(!is.na(`Você frequenta os sites do Conselho de Arquitetura e Urbanismo?`)) %>%
  count(`Você frequenta os sites do Conselho de Arquitetura e Urbanismo?`) %>%
  mutate(
    freq_absoluta = n,
    freq_relativa = scales::percent(n / sum(n), accuracy = 0.1)
  ) %>%
  mutate(
    label = str_c(freq_relativa, " (", freq_absoluta, ")")
  )


ggplot(siteconse) +
  aes(
    x = fct_reorder(`Você frequenta os sites do Conselho de Arquitetura e Urbanismo?`, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#CA1D1F", width = 0.7) +
  scale_fill_manual(values = cores_personalizadas, name = n)+
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "Frequência em sites do conselho", y = "Frequência") +
  theme_ble()
  
  
ggsave("sitesconse.pdf", width = 158, height = 93, units = "mm")

#sites das entidades

banco$`Você frequenta sites das entidades de Arquitetos e Urbanistas?` <- gsub("Outros sites profissionais", "Outros sites", banco$`Você frequenta sites das entidades de Arquitetos e Urbanistas?`)
banco$`Você frequenta sites das entidades de Arquitetos e Urbanistas?` <- gsub("Não frequento", "Nenhum", banco$`Você frequenta sites das entidades de Arquitetos e Urbanistas?`)


sitentidades <- banco %>%
  filter(!is.na(`Você frequenta sites das entidades de Arquitetos e Urbanistas?`)) %>%
  count(`Você frequenta sites das entidades de Arquitetos e Urbanistas?`) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

ggplot(sitentidades) +
  aes(
    x = fct_reorder(`Você frequenta sites das entidades de Arquitetos e Urbanistas?`, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "Frequência em sites das entidades", y = "Frequência") +
  theme_estat()

#ggsave("site_entidades.pdf", width = 158, height = 93, units = "mm")


#Áreas de informação de despertam mais interesse 


interesse <- banco %>%
  filter(!is.na(`Assinale as áreas de informações que lhe despertam maior interesse.`)) %>%
  count(`Assinale as áreas de informações que lhe despertam maior interesse.`) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(freq, " (", n, ")") %>% str_squish())

ggplot(interesse) +
  aes(
    x = fct_reorder(`Assinale as áreas de informações que lhe despertam maior interesse.`, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#CA1D1F", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = 0.01, hjust = 0.003,
    size = 3
  ) +
  labs(x = "Áreas de interesse", y = "Frequência") +
  scale_y_continuous(
    breaks = seq(0, 10416, 2604),
    expand = c(0, 0),
    limits = c(0, 10416)
  ) +
  scale_x_discrete(labels= c("Cultura \ne Lazer","Economia \ne Negócios", "Notícias \nLocais","Notícias \nInternacionais","Informática", "Outros" , "Notícias \nPoliticas","
Esportes", "Veículos"))+
  coord_flip()+
  theme_ble()




ggplot(interesse) +
  aes(
    x = fct_reorder(`Assinale as áreas de informações que lhe despertam maior interesse.`, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#CA1D1F", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = 0.01, hjust = 0.003,
    size = 3
  ) +
  labs(x = "Áreas de interesse", y = "Frequência") +
  scale_y_continuous(
    breaks = seq(0, 10416, 2604),
    limits = c(0, 10416)
  ) +
  scale_x_discrete(
    labels = c("Cultura \ne Lazer","Economia \ne Negócios", "Notícias \nLocais","Notícias \nInternacionais","Informática", "Outros" , "Notícias \nPolíticas","Esportes", "Veículos")
  ) +
  coord_flip() +
  theme_ble()





ggsave("areas_interesse.pdf", width = 158, height = 93, units = "mm")


#papel da política na sua vida 


politica <- banco %>%
  filter(!is.na(`Que papel tem a política na sua vida?`)) %>%
  count(`Que papel tem a política na sua vida?`) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )


politica$`Que papel tem a política na sua vida?` <- gsub("Muito Importante. Procuro discutir sobre política com meus colegas", "Muito importante", politica$`Que papel tem a política na sua vida?`)
politica$`Que papel tem a política na sua vida?` <- gsub("Não me interesso, Não entende ou não gosto de política", "Não me interesso /Não entendo", politica$`Que papel tem a política na sua vida?`)
politica$`Que papel tem a política na sua vida?` <- gsub("Não me preocupo com política ou não costumo discutir sobre política", "Não me preocupo", politica$`Que papel tem a política na sua vida?`)
politica$`Que papel tem a política na sua vida?` <- gsub("Importante. Eventualmente discuto sobre política com meus colegas", "Importante", politica$`Que papel tem a política na sua vida?`)


ggplot(politica) +
  aes(
    x = fct_reorder(`Que papel tem a política na sua vida?`, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3
  ) +
  labs(x = "Papel da política na sua vida", y = "Frequência") +
  scale_x_discrete(labels= c("Importante","Muito\nimportante", "Não me interesso\nNão discuto", "Não me\npreocupo", "Outros"))+
  theme_estat()

#nível de satisfação em relação à

teste <- banco[, c(
  "Status Social da profissão de arquiteto e urbanista",
  "Exercício da Profissão de Arquitetura e Urbanismo",
  "Rendimentos mensais da profissão de Arquitetura e Urbanismo",
  "Tecnologias de software disponíveis a profissão de Arquitetura e Urbanismo"
)]

test <- melt(test)

library(tidyr)

teste$Satisfação <- gsub("Exercício da Profissão de Arquitetura e Urbanismo", "Exercício da profissão", teste$Satisfação)


teste <- teste %>%
  pivot_longer(
    cols = c("Status Social da profissão de arquiteto e urbanista",
             "Exercício da Profissão de Arquitetura e Urbanismo",
             "Rendimentos mensais da profissão de Arquitetura e Urbanismo",
             "Tecnologias de software disponíveis a profissão de Arquitetura e Urbanismo"),
    names_to = "Satisfação",
    values_to = "Respostas"
  )

teste <- na.omit(teste)

# Agora, crie o gráfico com as adaptações necessárias
cores_personalizadas <- c("#CC9900","#CA1D1F","#086C75","#A11D21","#006606","#003366")
ggplot(teste, aes(x = Satisfação, fill = Respostas))+
  geom_bar(stat = "count", position = "fill") +
  scale_fill_manual(values = cores_estat, name = "Respostas")+
  labs(x = "Opções", y = "Frequência")+
  scale_x_discrete(labels= c("Tecnologias de\n software \ndisponíveis","Status social\n da profissão", "Rendimentos\nmensais", "Exercícios da\n profissão"))

#ggsave("satisfacao.pdf", width = 158, height = 93, units = "mm")



#Uso de computadores e comunicação


comp <- banco[, c(
  "Computadores PC (Desktops)",
  "Notebooks",
  "Tablets",
  "Smartphones",
  "Celular simples",
  "Outros dispositivos?"
)]


comp <- comp %>%
  rename(`Outros dispositivos` = `Outros dispositivos?`)
         
comp <- comp %>%
  pivot_longer(
    cols = c("Computadores PC (Desktops)",
             "Notebooks",
             "Tablets",
             "Smartphones",
             "Celular simples",
             "Outros dispositivos"),
    names_to = "Uso",
    values_to = "Respostas"
  )

comp <- comp %>%
  filter(!is.na(Respostas)) 

ggplot(comp, aes(x = Uso, fill = Respostas))+
  geom_bar(stat = "count", position = "fill") +
  scale_fill_manual(values = cores_estat, name = "Respostas")+
  labs(x = "Uso de aparelhos tecnológicos", y = "Frequência")+
  scale_x_discrete(labels= c("Computador \n(Desktops)","Notebooks", "Tablets", "Smartphones","Celular \nsimples","Outros \ndispositivos"))


comp$Respostas <- gsub("Não tenho acesso a outros dispositivos", "Não uso outros", comp$Respostas)

ggsave("uso_comp.pdf", width = 158, height = 93, units = "mm")











