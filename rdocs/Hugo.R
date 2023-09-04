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



#Em relação aos nichos de mercado de atuação do arquiteto e urbanista,
table(banco1$`Na sua opinião o mercado de trabalho para arquitetura e urbanismo está:`)
table(banco1$`Imobiliário está`)
table(banco1$`Hotelaria e Turismo está`)
table(banco1$`Hospitalar e Saúde está`)   ?????????
  
  
  table(banco1$`Possui pessoa(s)jurídica(s) na área da arquitetura e urbanismo?`)

?????????
  
  
  
  #############################################Atuação em A\U
  table(banco1$`Trabalha na área de Arquitetura e Urbanismo?`)



contagem <- banco1 %>% 
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


#######################################Áreas de atuação

table(banco1$`Quais as suas áreas de atuação nos últimos 2 anos?`)

classes <- banco1 %>%
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
                              '0','Arquitetura e \nUrbanismos \nExecução',
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

###################################Tipos de projetos executados

table(banco1$`Nos projetos arquitetônicos que realiza, você executa predominantemente:`)

####################################Jornada semanal- A\U

table(banco1$`Quantas horas por semana você trabalha com arquitetura e urbanismo?`)

######################################Tipos de contratantes

table(banco1$`Quais tipos de contratantes você trabalhou nos últimos 2 anos?`)

classes <- banco1 %>%
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

########################################Opinião sobre tendências

########################################Áreas inexploradas na A\U

table(banco1$`Você considera que há outras áreas do mercado que ainda são inexploradas pelos arquitetos e urbanistas?`)

#########################################Outra atividade fora da A\U

table(banco1$`Você está trabalhando em outra atividade fora da área da arquitetura e urbanismo?`)

#########################################Jornada semanal- outras áreas

table(banco1$`Quantas horas por semana você trabalha com outra atividade fora da área da arquitetura e urbanismo?`)

