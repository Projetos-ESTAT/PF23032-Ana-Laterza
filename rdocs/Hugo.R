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


col_nomes<- read_xlsx('banco/colnames.xlsx', col_names = F)

banco1 <- read_xlsx('banco/análise-para-ESTAT.xlsx', col_names = T, sheet = 3)

banco1 <- banco1[-c(1,2),-c(1,2,3)]
names(banco1) <- col_nomes

  
  
  #############################################Atuação em A\U
  table(banco1$`Trabalha na área de Arquitetura e Urbanismo?`)

banco2<- banco1 %>%
  filter(`Trabalha na área de Arquitetura e Urbanismo?`!= 0)

table(banco2$`Trabalha na área de Arquitetura e Urbanismo?`)

contagem <- banco2 %>% 
  group_by(`Trabalha na área de Arquitetura e Urbanismo?`) %>%
  summarise(Freq = n()) %>%
  mutate(Prop = round(100*(Freq/sum(Freq)), 2)) %>%
  arrange(desc(`Trabalha na área de Arquitetura e Urbanismo?`)) %>%
  mutate(posicao = cumsum(Prop) - 0.5*Prop)

ggplot(contagem) +
  aes(x = factor(""), y = Prop , fill = factor(`Trabalha na área de Arquitetura e Urbanismo?`)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(
    aes(x = 1.8, y = posicao, label = paste0(Prop, "%")),
    color = "black"
  ) +
  theme_void() +
  theme(legend.position = "top") +
  scale_fill_manual(values = cores_estat, name = 'Você está trabalhando em outra atividade \nfora da área da arquitetura e urbanismo?')
ggsave("graficos_hugo/setor_AtuaçãoAU.pdf", width = 158, height = 93, units = "mm")


######################################Empresas de A\U

table(banco1$`Possui pessoa(s)jurídica(s) na área da arquitetura e urbanismo?`)

banco2<- banco1 %>%
  filter(`Possui pessoa(s)jurídica(s) na área da arquitetura e urbanismo?`!= 0)

table(banco2$`Possui pessoa(s)jurídica(s) na área da arquitetura e urbanismo?`)

contagem <- banco2 %>% 
  group_by(`Possui pessoa(s)jurídica(s) na área da arquitetura e urbanismo?`) %>%
  summarise(Freq = n()) %>%
  mutate(Prop = round(100*(Freq/sum(Freq)), 2)) %>%
  arrange(desc(`Possui pessoa(s)jurídica(s) na área da arquitetura e urbanismo?`)) %>%
  mutate(posicao = cumsum(Prop) - 0.5*Prop)

ggplot(contagem) +
  aes(x = factor(""), y = Prop , fill = factor(`Possui pessoa(s)jurídica(s) na área da arquitetura e urbanismo?`)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(
    aes(x = 1.8, y = posicao, label = paste0(Prop, "%")),
    color = "black"
  ) +
  theme_void() +
  theme(legend.position = "top") +
  scale_fill_manual(values = cores_estat, name = 'Possui pessoa(s)jurídica(s) \nna área da \narquitetura e urbanismo?', label= c("Não", 
                                                                                                                               "Sim, Mista com arquitetos e \nUrbanistas e \noutras profissões",
                                                                                                                               "Sim, Uniprofissionais \napenas com Arquitetos \ne Urbanistas"))
ggsave("graficos_hugo/setor_EmpresasAU.pdf", width = 158, height = 93, units = "mm")


#######################################Áreas de atuação

table(banco1$`Quais as suas áreas de atuação nos últimos 2 anos?`)

banco2<- banco1 %>%
  filter(`Quais as suas áreas de atuação nos últimos 2 anos?`!= 0)

table(banco2$`Quais as suas áreas de atuação nos últimos 2 anos?`)


classes <- banco2 %>%
  filter(!is.na(`Quais as suas áreas de atuação nos últimos 2 anos?`)) %>%
  count(`Quais as suas áreas de atuação nos últimos 2 anos?`) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

ggplot(classes) +
  aes(x = fct_reorder(`Quais as suas áreas de atuação nos últimos 2 anos?`, n, .desc=T), y = n, label = label) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  labs(x = "Área de atuação nos últimos 2 anos", y = "Frequência") +
  scale_x_discrete(labels = c('Arquitetura \nde Interiores',
                              'Arquitetura \nUrbanismo \nConcepção',
                              'Arquitetura e \nUrbanismos \nExecução',
                              'Outra',
                              'Arquitetura \nPaisagística',
                              'Ensino',
                              'Planejamento Urbano\n e Regional',
                              'Sistemas \nConstrutivos \ne Estruturais',
                              'Patrimônio \nHistórico',
                              'Instalações \nEquipamentos \nElétricos',
                              'Engenharia de \nSegurança \ndo Trabalho',
                              'Topografia',
                              'Geoprocessamento e\n Correlatas',
                              'Tecnologia e \nResistência \ndos Materiais')
                   ) +
  theme_estat()
ggsave("graficos_hugo/colunas-uni-freq_AreaAtuacao.pdf", width = 475, height = 275, units = "mm")


###################################Referência de honorários

table(banco1$`Qual o valor de referência que você usa para dimensionar os honorários para elaboração de projeto arquitetônico?`)

banco2<- banco1 %>%
  filter(`Qual o valor de referência que você usa para dimensionar os honorários para elaboração de projeto arquitetônico?`!= 0)

table(banco2$`Qual o valor de referência que você usa para dimensionar os honorários para elaboração de projeto arquitetônico?`)

classes <- banco2 %>%
  filter(!is.na(`Qual o valor de referência que você usa para dimensionar os honorários para elaboração de projeto arquitetônico?`)) %>%
  count(`Qual o valor de referência que você usa para dimensionar os honorários para elaboração de projeto arquitetônico?`) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

ggplot(classes) +
  aes(x = fct_reorder(`Qual o valor de referência que você usa para dimensionar os honorários para elaboração de projeto arquitetônico?`, n, .desc=T), y = n, label = label) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  labs(x = "Honorário", y = "Frequência") +
  scale_x_discrete(labels = c("Valos m²",
                              "Outra",
                              "Tabela de honorários \ndo CAU/BR",
                              "C.U.B.",
                              "Tabela IAB")
  ) +
  theme_estat()
ggsave("graficos_hugo/colunas-uni-freq_honorario.pdf", width = 158, height = 93, units = "mm")

###################################Tipos de projetos executados

table(banco1$`Nos projetos arquitetônicos que realiza, você executa predominantemente:`)


banco2<- banco1 %>%
  filter(`Nos projetos arquitetônicos que realiza, você executa predominantemente:`!= 0)

table(banco2$`Nos projetos arquitetônicos que realiza, você executa predominantemente:`)

classes <- banco2 %>%
  filter(!is.na(`Nos projetos arquitetônicos que realiza, você executa predominantemente:`)) %>%
  count(`Nos projetos arquitetônicos que realiza, você executa predominantemente:`) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

ggplot(classes) +
  aes(x = fct_reorder(`Nos projetos arquitetônicos que realiza, você executa predominantemente:`, n, .desc=T), y = n, label = label) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  labs(x = "Projeto executado", y = "Frequência") +
  scale_x_discrete(labels = c("Projeto \nde aprovação \nou básico",
                              "Projeto \nExecutivo",
                              "Execução \nde obras",
                              "Fiscalização \ndireção de obras",
                              "Coordenação \ndos projetos \ncomplementares",
                              "Autoria dos \nprojetos \ncomplementares")
                   ) +
  theme_estat()
ggsave("graficos_hugo/colunas-uni-freq_executa.pdf", width = 258, height = 152, units = "mm")

####################################Jornada semanal- A\U

table(banco1$`Quantas horas por semana você trabalha com arquitetura e urbanismo?`)

banco2<- banco1 %>%
  filter(`Quantas horas por semana você trabalha com arquitetura e urbanismo?`!= 0)


table(banco2$`Quantas horas por semana você trabalha com arquitetura e urbanismo?`)

classes <- banco2 %>%
  filter(!is.na(`Quantas horas por semana você trabalha com arquitetura e urbanismo?`)) %>%
  count(`Quantas horas por semana você trabalha com arquitetura e urbanismo?`) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

ggplot(classes) +
  aes(x = fct_reorder(`Quantas horas por semana você trabalha com arquitetura e urbanismo?`, n, .desc=T), y = n, label = label) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  labs(x = "Tempo trabalhando com A&U", y = "Frequência") +
  scale_x_discrete(labels = c("Mais de 40 horas",
                              "De 30 a 40 horas",
                              "Trabalho \nesporadicamente",
                              "De 20 a 30 horas",
                              "De 10 a 20 horas",
                              "Não trabalho \ncom Arquitetura e Urbanismo",
                              "Até 10 horas")
                              ) +
  theme_estat()
ggsave("graficos_hugo/colunas-uni-freq_horasAU.pdf", width = 258, height = 152, units = "mm")



######################################Tipos de contratantes

table(banco1$`Quais tipos de contratantes você trabalhou nos últimos 2 anos?`)

banco2<- banco1 %>%
  filter(`Quais tipos de contratantes você trabalhou nos últimos 2 anos?`!= 0)


table(banco2$`Quais tipos de contratantes você trabalhou nos últimos 2 anos?`)


classes <- banco2 %>%
  filter(!is.na(`Quais tipos de contratantes você trabalhou nos últimos 2 anos?`)) %>%
  count(`Quais tipos de contratantes você trabalhou nos últimos 2 anos?`) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

ggplot(classes) +
  aes(x = fct_reorder(`Quais tipos de contratantes você trabalhou nos últimos 2 anos?`, n, .desc=T), y = n, label = label) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  labs(x = "Tipos de contratantes nos últimos 2 anos", y = "Frequência") +
  theme_estat()
ggsave("graficos_hugo/colunas-uni-freq_TipoContratante.pdf", width = 158, height = 93, units = "mm")

#####################################Opinião sobre o mercado

table(banco1$`Na sua opinião o mercado de trabalho para arquitetura e urbanismo está:`)


banco2<- banco1 %>%
  filter(`Na sua opinião o mercado de trabalho para arquitetura e urbanismo está:`!= 0)


table(banco2$`Na sua opinião o mercado de trabalho para arquitetura e urbanismo está:`)


classes <- banco2 %>%
  filter(!is.na(`Na sua opinião o mercado de trabalho para arquitetura e urbanismo está:`)) %>%
  count(`Na sua opinião o mercado de trabalho para arquitetura e urbanismo está:`) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

ggplot(classes) +
  aes(x = fct_reorder(`Na sua opinião o mercado de trabalho para arquitetura e urbanismo está:`, n, .desc=T), y = n, label = label) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  labs(x = "Tipos de contratantes nos últimos 2 anos", y = "Frequência") +
  theme_estat()
ggsave("graficos_hugo/colunas-uni-freq_opiniaomercado.pdf", width = 158, height = 93, units = "mm")

########################################Opinião sobre tendências

table(banco1$`Imobiliário está`)
table(banco1$`Hotelaria e Turismo está`)
table(banco1$`Hospitalar e Saúde está`) 

banco2<- banco1 %>%
  filter(`Imobiliário está`!= 0)

banco2<- banco2 %>%
  filter(`Hotelaria e Turismo está`!= 0)

banco2<- banco2 %>%
  filter(`Hospitalar e Saúde está`!= 0)




trans_drv <- banco2 %>%
  mutate(`Imobiliário está` = case_when(
    `Imobiliário está` %>% str_detect("Em expansão") ~ "Em expansão",
    `Imobiliário está` %>% str_detect("Em retração") ~ "Em retração",
    `Imobiliário está` %>% str_detect("Inalterado") ~ "Inalterado",
    `Imobiliário está` %>% str_detect("Sem resposta") ~ "Sem resposta"
  )) %>%
  group_by(`Imobiliário está`, `Hotelaria e Turismo está`) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = freq %>% percent()
  )

porcentagens <- str_c(trans_drv$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(trans_drv$freq, " (", porcentagens, ")"))

ggplot(trans_drv) +
  aes(
    x = fct_reorder(trans, freq, .desc = T), y = freq,
    fill = drv, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Transmissão", y = "Frequência") +
  theme_estat()
ggsave("colunas-bi-freq.pdf", width = 158, height = 93, units = "mm")




########################################Áreas inexploradas na A\U

table(banco1$`Você considera que há outras áreas do mercado que ainda são inexploradas pelos arquitetos e urbanistas?`)

banco2<- banco1 %>%
  filter(`Você considera que há outras áreas do mercado que ainda são inexploradas pelos arquitetos e urbanistas?`!= 0)

table(banco2$`Você considera que há outras áreas do mercado que ainda são inexploradas pelos arquitetos e urbanistas?`)

contagem <- banco2 %>% 
  group_by(`Você considera que há outras áreas do mercado que ainda são inexploradas pelos arquitetos e urbanistas?`) %>%
  summarise(Freq = n()) %>%
  mutate(Prop = round(100*(Freq/sum(Freq)), 2)) %>%
  arrange(desc(`Você considera que há outras áreas do mercado que ainda são inexploradas pelos arquitetos e urbanistas?`)) %>%
  mutate(posicao = cumsum(Prop) - 0.5*Prop)

ggplot(contagem) +
  aes(x = factor(""), y = Prop , fill = factor(`Você considera que há outras áreas do mercado que ainda são inexploradas pelos arquitetos e urbanistas?`)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(
    aes(x = 1.8, y = posicao, label = paste0(Prop, "%")),
    color = "black"
  ) +
  theme_void() +
  theme(legend.position = "top") +
  scale_fill_manual(values = cores_estat, name = 'Você considera que há outras áreas do mercado que ainda são \ninexploradas pelos arquitetos e urbanistas?')
ggsave("graficos_hugo/setor_Areasinexploradas.pdf", width = 158, height = 93, units = "mm")


#########################################Outra atividade fora da A\U

table(banco1$`Você está trabalhando em outra atividade fora da área da arquitetura e urbanismo?`)

banco2<- banco1 %>%
  filter(`Você está trabalhando em outra atividade fora da área da arquitetura e urbanismo?`!= 0)

table(banco2$`Você está trabalhando em outra atividade fora da área da arquitetura e urbanismo?`)

contagem <- banco2 %>% 
  group_by(`Você está trabalhando em outra atividade fora da área da arquitetura e urbanismo?`) %>%
  summarise(Freq = n()) %>%
  mutate(Prop = round(100*(Freq/sum(Freq)), 2)) %>%
  arrange(desc(`Você está trabalhando em outra atividade fora da área da arquitetura e urbanismo?`)) %>%
  mutate(posicao = cumsum(Prop) - 0.5*Prop)

ggplot(contagem) +
  aes(x = factor(""), y = Prop , fill = factor(`Você está trabalhando em outra atividade fora da área da arquitetura e urbanismo?`)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(
    aes(x = 1.8, y = posicao, label = paste0(Prop, "%")),
    color = "black"
  ) +
  theme_void() +
  theme(legend.position = "top") +
  scale_fill_manual(values = cores_estat, name = 'Você está trabalhando em outra atividade \nfora da área da arquitetura e urbanismo?')
ggsave("graficos_hugo/setor_Atuafora.pdf", width = 158, height = 93, units = "mm")


#########################################Jornada semanal- outras áreas

table(banco1$`Quantas horas por semana você trabalha com outra atividade fora da área da arquitetura e urbanismo?`)


banco2<- banco1 %>%
  filter(`Quantas horas por semana você trabalha com outra atividade fora da área da arquitetura e urbanismo?`!= 0)


table(banco2$`Quantas horas por semana você trabalha com outra atividade fora da área da arquitetura e urbanismo?`)

classes <- banco2 %>%
  filter(!is.na(`Quantas horas por semana você trabalha com outra atividade fora da área da arquitetura e urbanismo?`)) %>%
  count(`Quantas horas por semana você trabalha com outra atividade fora da área da arquitetura e urbanismo?`) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

ggplot(classes) +
  aes(x = fct_reorder(`Quantas horas por semana você trabalha com outra atividade fora da área da arquitetura e urbanismo?`, n, .desc=T), y = n, label = label) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  labs(x = "Tempo trabalhando fora de A&U", y = "Frequência") +
  scale_x_discrete(labels = c("Trabalho \nesporadicamente",
                              "Até 10 horas",
                              "Mais de 40 horas",
                              "De 10 a 20 horas",
                              "De 30 a 40 horas",
                              "De 20 a 30 horas",
                              "Não trabalho \ncom Arquitetura e Urbanismo")
  ) +
  theme_estat()
ggsave("graficos_hugo/colunas-uni-freq_horasfora.pdf", width = 258, height = 152, units = "mm")


