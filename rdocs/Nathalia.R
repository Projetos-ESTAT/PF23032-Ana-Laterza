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


#### criando banco usando o código do Bruno ####

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
df$`n. desvios`[is.na(df$`n. desvios`)] <- 0

df <- df |>
  mutate(across(1, as.Date)) %>%  # Converte a primeira coluna em formato "as_date"
  mutate(across(-1, as.factor)) 

df <- df |>
  filter(!(`n. desvios` == 0 & is.na(`100% norma`)))

levels(df$`100% norma`) <- c("norma", "Não norma")
df$`100% norma`[is.na(df$`100% norma`)] <- factor("Não norma")

# removendo colunas desinteressantes:
df <- df[, -c(1,2,3,80)]



#### transformando as subperguntas em valores numéricos ####

## ordem usada
# 1 = 'Nunca'
# 2 = 'Raramente'
# 3 = 'às vezes'
# 4 = 'Quase diariamente'
# 5 = 'Diariamente'

### Com que frequência você tem acesso a:
df$`Publicações acadêmicas de Arquitetura` <- factor(df$`Publicações acadêmicas de Arquitetura`, 
                         labels = c(1, 2, 3, 4, 5),
                         levels = c('Nunca', 'Raramente', 'às vezes', 'Quase diariamente', 'Diariamente'))
df$`Revistas de Arquitetura` <- factor(df$`Revistas de Arquitetura`, 
                                       labels = c(1, 2, 3, 4, 5),
                                       levels = c('Nunca', 'Raramente', 'às vezes', 'Quase diariamente', 'Diariamente'))
df$`Livros Técnicos em Arquitetura e Urbanismo` <- factor(df$`Livros Técnicos em Arquitetura e Urbanismo`, 
                                       labels = c(1, 2, 3, 4, 5),
                                       levels = c('Nunca', 'Raramente', 'às vezes', 'Quase diariamente', 'Diariamente'))
df$`Sites de Arquitetura e Urbanismo` <- factor(df$`Sites de Arquitetura e Urbanismo`, 
                                                labels = c(1, 2, 3, 4, 5),
                                                levels = c('Nunca', 'Raramente', 'às vezes', 'Quase diariamente', 'Diariamente'))
df$Jornais <- factor(df$Jornais, 
                     labels = c(1, 2, 3, 4, 5),
                     levels = c('Nunca', 'Raramente', 'às vezes', 'Quase diariamente', 'Diariamente'))
df$Revistas <- factor(df$Revistas, 
                      labels = c(1, 2, 3, 4, 5),
                      levels = c('Nunca', 'Raramente', 'às vezes', 'Quase diariamente', 'Diariamente'))
df$`TV aberta` <- factor(df$`TV aberta`, 
                         labels = c(1, 2, 3, 4, 5),
                         levels = c('Nunca', 'Raramente', 'às vezes', 'Quase diariamente', 'Diariamente'))
df$`TV a Cabo` <- factor(df$`TV a Cabo`, 
                         labels = c(1, 2, 3, 4, 5),
                         levels = c('Nunca', 'Raramente', 'às vezes', 'Quase diariamente', 'Diariamente'))
df$Internet <- factor(df$Internet, 
                      labels = c(1, 2, 3, 4, 5),
                      levels = c('Nunca', 'Raramente', 'às vezes', 'Quase diariamente', 'Diariamente'))
df$Livros <- factor(df$Livros, 
                    labels = c(1, 2, 3, 4, 5),
                    levels = c('Nunca', 'Raramente', 'às vezes', 'Quase diariamente', 'Diariamente'))
df$`Rádio AM/FM` <- factor(df$`Rádio AM/FM`, 
                           labels = c(1, 2, 3, 4, 5),
                           levels = c('Nunca', 'Raramente', 'às vezes', 'Quase diariamente', 'Diariamente'))
df$`Outros meios de comunicação?` <- factor(df$`Outros meios de comunicação?`, 
                                            labels = c(1, 2, 3, 4, 5),
                                            levels = c('Nunca', 'Raramente', 'às vezes', 'Quase diariamente', 'Diariamente'))
df[c(11:22)] <- lapply(df[c(11:22)], as.numeric)

### Quanto ao uso de computadores e comunicação móvel, com que frequência você tem acesso a:
df$`Computadores PC (Desktops)` <- factor(df$`Computadores PC (Desktops)`, 
                                            labels = c(1, 2, 3, 4, 5),
                                            levels = c('Nunca', 'Raramente', 'às vezes', 'Quase diariamente', 'Diariamente'))
df$Notebooks <- factor(df$Notebooks,
                       labels = c(1, 2, 3, 4, 5),
                       levels = c('Nunca', 'Raramente', 'às vezes', 'Quase diariamente', 'Diariamente'))
df$Tablets <- factor(df$Tablets, 
                     labels = c(1, 2, 3, 4, 5),
                     levels = c('Nunca', 'Raramente', 'às vezes', 'Quase diariamente', 'Diariamente'))
df$Smartphones <- factor(df$Smartphones, 
                         labels = c(1, 2, 3, 4, 5),
                         levels = c('Nunca', 'Raramente', 'às vezes', 'Quase diariamente', 'Diariamente'))
df$`Celular simples` <- factor(df$`Celular simples`, 
                                          labels = c(1, 2, 3, 4, 5),
                                          levels = c('Nunca', 'Raramente', 'às vezes', 'Quase diariamente', 'Diariamente'))
df$`Outros dispositivos?` <- factor(df$`Outros dispositivos?`, 
                                          labels = c(1, 2, 3, 4, 5),
                                          levels = c('Nunca', 'Raramente', 'às vezes', 'Quase diariamente', 'Diariamente'))
df[c(23:28)] <- lapply(df[c(23:28)], as.numeric)

## ordem usada
# 1 = 'Não conheço'
# 2 = 'Muito Ruim'
# 3 = 'Ruim'
# 4 = 'Bom'
# 5 = 'Muito bom'

### Quanto aos softwares profissionais da área de arquitetura e urbanismo, como classifica seus conhecimentos em:
df$`Desenho por computador?` <- factor(df$`Desenho por computador?`, 
                                    labels = c(1, 2, 3, 4, 5),
                                    levels = c('Não conheço', 'Muito Ruim', 'Ruim', 'Bom', 'Muito bom'))
df$`Geoprocessamento?` <- factor(df$`Geoprocessamento?`, 
                                       labels = c(1, 2, 3, 4, 5),
                                       levels = c('Não conheço', 'Muito Ruim', 'Ruim', 'Bom', 'Muito bom'))
df$`Outros softwares profissionais?` <- factor(df$`Outros softwares profissionais?`, 
                                       labels = c(1, 2, 3, 4, 5),
                                       levels = c('Não conheço', 'Muito Ruim', 'Ruim', 'Bom', 'Muito bom'))
df[c(30:32)] <- lapply(df[c(30:32)], as.numeric)

## ordem usada
# 1 = 'Não'
# 2 = 'Sim'

### Sobre redes sociais, você frequenta:
df$Facebook <- factor(df$Facebook, 
                      labels = c(1,2),
                      levels = c('Não', 'Sim'))
df$Twitter <- factor(df$Twitter, 
                      labels = c(1,2),
                      levels = c('Não', 'Sim'))
df$Linkedin <- factor(df$Linkedin, 
                      labels = c(1,2),
                      levels = c('Não', 'Sim'))
df$Instagram <- factor(df$Instagram, 
                      labels = c(1,2),
                      levels = c('Não', 'Sim'))
df$`Outras redes` <- factor(df$`Outras redes`, 
                       labels = c(1,2),
                       levels = c('Não', 'Sim'))
df[c(33:37)] <- lapply(df[c(33:37)], as.numeric)

## ordem usada
# 1 = 'Não falo nenhum idioma estrangeiro'
# 2 = 'Básico'
# 3 = 'Intermediário'
# 4 = 'Avançado'
# 5 = 'Fluente'

### Com relação ao domínio de idiomas estrangeiros, como você considera os seus conhecimentos em:
df$Inglês <- factor(df$Inglês, 
                    labels = c(1,2,3,4,5),
                    levels = c('Não falo nenhum idioma estrangeiro', 'Básico', "Intermediário", 'Avançado', 'Fluente'))
df$Francês <- factor(df$Francês, 
                    labels = c(1,2,3,4,5),
                    levels = c('Não falo nenhum idioma estrangeiro', 'Básico', "Intermediário", 'Avançado', 'Fluente'))
df$Espanhol <- factor(df$Espanhol, 
                    labels = c(1,2,3,4,5),
                    levels = c('Não falo nenhum idioma estrangeiro', 'Básico', "Intermediário", 'Avançado', 'Fluente'))
df$`Outras opções` <- factor(df$`Outras opções`, 
                    labels = c(1,2,3,4,5),
                    levels = c('Não falo nenhum idioma estrangeiro', 'Básico', "Intermediário", 'Avançado', 'Fluente'))
df[c(40:43)] <- lapply(df[c(40:43)], as.numeric)


### Possui alguma deficiência?
# juntando as colunas

df$`se "Sim, física", qual?` = case_when(
  df$`se "Sim, física", qual?` %in% c("Outra deficiência motora", "Deficiência visual", "Deficiência auditiva","Pessoa em cadeira de rodas Deficiência da fala.") ~ "Sim, física"
)

df$`Possui alguma deficiência?` <- factor(df$`Possui alguma deficiência?`,
                                          levels = c("Nenhuma", "Sim, sensorial", "Sim, mental", "Sim, intelectual"),
                                          labels = c("Nenhuma", "Sim, sensorial", "Sim, mental", "Sim, intelectual"))

df$`Possui deficiência` <- ifelse(!is.na(df$`se "Sim, física", qual?`), df$`se "Sim, física", qual?`, as.character(df$`Possui alguma deficiência?`))
df$`Possui deficiência` <- as.factor(df$`Possui deficiência`)

## ordem usada
# 1 = 'Totalmente Insatisfeito'
# 2 = 'Parcialmente Insatisfeito'
# 3 = 'Nem satisfeito e nem insatisfeito'
# 4 = 'Parcialmente Satisfeito'
# 5 = 'Totalmente Satisfeito'

### Qual é seu nível de satisfação em relação a:
df$`Status Social da profissão de arquiteto e urbanista` <- factor(df$`Status Social da profissão de arquiteto e urbanista`, 
                                                                   labels = c(1,2,3,4,5),
                                                                   levels = c('Totalmente Insatisfeito', 'Parcialmente Insatisfeito', 
                                                                              "Nem satisfeito e nem insatisfeito", 
                                                                              'Parcialmente Satisfeito', 'Totalmente Satisfeito'))
df$`Exercício da Profissão de Arquitetura e Urbanismo` <- factor(df$`Exercício da Profissão de Arquitetura e Urbanismo`, 
                                                                   labels = c(1,2,3,4,5),
                                                                   levels = c('Totalmente Insatisfeito', 'Parcialmente Insatisfeito', 
                                                                              "Nem satisfeito e nem insatisfeito", 
                                                                              'Parcialmente Satisfeito', 'Totalmente Satisfeito'))
df$`Rendimentos mensais da profissão de Arquitetura e Urbanismo` <- factor(df$`Rendimentos mensais da profissão de Arquitetura e Urbanismo`, 
                                                                 labels = c(1,2,3,4,5),
                                                                 levels = c('Totalmente Insatisfeito', 'Parcialmente Insatisfeito', 
                                                                            "Nem satisfeito e nem insatisfeito", 
                                                                            'Parcialmente Satisfeito', 'Totalmente Satisfeito'))
df$`Tecnologias de software disponíveis a profissão de Arquitetura e Urbanismo` <- factor(df$`Tecnologias de software disponíveis a profissão de Arquitetura e Urbanismo`, 
                                                                           labels = c(1,2,3,4,5),
                                                                           levels = c('Totalmente Insatisfeito', 'Parcialmente Insatisfeito', 
                                                                                      "Nem satisfeito e nem insatisfeito", 
                                                                                      'Parcialmente Satisfeito', 'Totalmente Satisfeito'))
df[c(50:53)] <- lapply(df[c(50:53)], as.numeric)


## ordem usada
# 1 = 'Sem resposta'
# 2 = 'Em retração'
# 3 = 'Inalterado'
# 4 = 'Em expansão'

### Em relação aos seguintes nichos de mercado de atuação do arquiteto e urbanista, você diria que o nicho:
df$`Imobiliário está` <- factor(df$`Imobiliário está`, 
                    labels = c(1,2,3,4),
                    levels = c('Sem resposta', 'Em retração', "Inalterado", 'Em expansão'))
df$`Hotelaria e Turismo está` <- factor(df$`Hotelaria e Turismo está`, 
                                labels = c(1,2,3,4),
                                levels = c('Sem resposta', 'Em retração', "Inalterado", 'Em expansão'))
df$`Hospitalar e Saúde está` <- factor(df$`Hospitalar e Saúde está`, 
                                        labels = c(1,2,3,4),
                                        levels = c('Sem resposta', 'Em retração', "Inalterado", 'Em expansão'))
df[c(56:58)] <- lapply(df[c(56:58)], as.numeric)

#### adicionando as médias ####

df$media_1 <- rowMeans(df[ , c(11:22)], na.rm=TRUE)
df$media_2 <- rowMeans(df[ , c(23:28)], na.rm=TRUE)
df$media_3 <- rowMeans(df[ , c(30:32)], na.rm=TRUE)
df$media_4 <- rowMeans(df[ , c(33:37)], na.rm=TRUE)
df$media_5 <- rowMeans(df[ , c(40:43)], na.rm=TRUE)
df$media_6 <- rowMeans(df[ , c(50:53)], na.rm=TRUE)
df$media_7 <- rowMeans(df[ , c(56:58)], na.rm=TRUE)

#### novo banco ####
banco_m <- df %>% 
  reframe(
    "n. devios" = `n. desvios`, "100% norma" = `100% norma`,
    "Trabalho outra área"=`Você está trabalhando em outra atividade fora da área da arquitetura e urbanismo?`,
    "Fonte de renda"= case_when(
      `Acerca de sua(s) fonte(s) de renda?` == 'Autônomo (empreendedor em áreas ligadas a Arquitetura e Urbanismo)' ~ "Autônomo em AU",
      `Acerca de sua(s) fonte(s) de renda?` == 'Assalariado (setor público em áreas ligadas a Arquitetura e Urbanismo)' ~ "Assalariado público em AU",
      `Acerca de sua(s) fonte(s) de renda?` == 'Outras fontes' ~ "Outras fontes",
      `Acerca de sua(s) fonte(s) de renda?` == 'Autônomo (empreendedor em áreas não ligadas a Arquitetura e Urbanismo)' ~ "Autônomo fora AU",
      `Acerca de sua(s) fonte(s) de renda?` == 'Assalariado (setor privado em áreas ligadas a Arquitetura e Urbanismo)' ~ "Assalariado privado em AU",
      `Acerca de sua(s) fonte(s) de renda?` == 'Empresário (em áreas ligadas a Arquitetura e Urbanismo)' ~ "Empresário em AU",
      `Acerca de sua(s) fonte(s) de renda?` == 'Empresário (outras áreas)' ~ "Empresário fora AU",
      `Acerca de sua(s) fonte(s) de renda?` == 'Assalariado (sem carteira em áreas não ligadas a Arquitetura e Urbanismo)' ~ "Assalariado sem carteira fora AU",
      `Acerca de sua(s) fonte(s) de renda?` == 'Assalariado (setor público em áreas não ligadas a Arquitetura e Urbanismo)' ~ "Assalariado público fora AU",
      `Acerca de sua(s) fonte(s) de renda?` == 'Seguro Desemprego' ~ "Seguro desemprego",
      `Acerca de sua(s) fonte(s) de renda?` == 'Aposentado ou Pensionistas' ~ "Aposentado ou Pensionista",
      `Acerca de sua(s) fonte(s) de renda?` == 'Renda Proveniente de Aluguel' ~ "Renda vindo de aluguel"
    ),
    "Imóvel próprio" = `Possui imóvel próprio?`, "Carro próprio" = `Possui carro próprio?`,
    "Renda individual" = case_when(
      `Qual a sua renda mensal individual, aproximadamente?` == "Nenhuma renda." ~ "Sem renda",
      `Qual a sua renda mensal individual, aproximadamente?` == "Até 1 salário mínimo (até R$ 998,00)." ~ "Até 1 S.M.",
      `Qual a sua renda mensal individual, aproximadamente?` == "De 1 a 3 salários mínimos (de R$ 998,01 até R$ 2.994,00)." ~ "1 a 3 S.M.",
      `Qual a sua renda mensal individual, aproximadamente?` == "De 3 a 6 salários mínimos (de R$ 2.994,01 até R$ 5.988,00)." ~ "3 a 6 S.M.",
      `Qual a sua renda mensal individual, aproximadamente?` == "De 6 a 9 salários mínimos (de R$ 5.988,01 até R$ 8.982,00)." ~ "6 a 9 S.M.",
      `Qual a sua renda mensal individual, aproximadamente?` == "De 9 a 12 salários mínimos (de R$ 8.982,01 até R$ 11.976,00)." ~ "9 a 12 S.M.",
      `Qual a sua renda mensal individual, aproximadamente?` == "De 12 a 15 salários mínimos (de R$ 11.976,01 até R$ 14.970,00)." ~ "12 a 15 S.M.",
      `Qual a sua renda mensal individual, aproximadamente?` == "Mais de 15 salários mínimos (mais de R$ 14.970,01)." ~ "Mais de 15 S.M."
    ),
    "Renda familiar" = case_when(
      `Somando a sua renda com a renda das pessoas que moram com você, quanto é, aproximadamente, a renda familiar mensal?` == "Nenhuma renda." ~ "Sem renda",
      `Somando a sua renda com a renda das pessoas que moram com você, quanto é, aproximadamente, a renda familiar mensal?` == "Até 1 salário mínimo (até R$ 998,00)." ~ "Até 1 S.M.",
      `Somando a sua renda com a renda das pessoas que moram com você, quanto é, aproximadamente, a renda familiar mensal?` == "De 1 a 3 salários mínimos (de R$ 998,01 até R$ 2.994,00)." ~ "1 a 3 S.M.",
      `Somando a sua renda com a renda das pessoas que moram com você, quanto é, aproximadamente, a renda familiar mensal?` == "De 3 a 6 salários mínimos (de R$ 2.994,01 até R$ 5.988,00)." ~ "3 a 6 S.M.",
      `Somando a sua renda com a renda das pessoas que moram com você, quanto é, aproximadamente, a renda familiar mensal?` == "De 6 a 9 salários mínimos (de R$ 5.988,01 até R$ 8.982,00)." ~ "6 a 9 S.M.",
      `Somando a sua renda com a renda das pessoas que moram com você, quanto é, aproximadamente, a renda familiar mensal?` == "De 9 a 12 salários mínimos (de R$ 8.982,01 até R$ 11.976,00)." ~ "9 a 12 S.M.",
      `Somando a sua renda com a renda das pessoas que moram com você, quanto é, aproximadamente, a renda familiar mensal?` == "De 12 a 15 salários mínimos (de R$ 11.976,01 até R$ 14.970,00)." ~ "12 a 15 S.M.",
      `Somando a sua renda com a renda das pessoas que moram com você, quanto é, aproximadamente, a renda familiar mensal?` == "Mais de 15 salários mínimos (mais de R$ 14.970,01)." ~ "Mais de 15 S.M."
    ),
    "Participação eventos AU" = case_when(
      `Como é a sua participação em feiras, seminários, congressos e afins na área da arquitetura e urbanismo?` == "Frequenta algumas" ~ "Algumas",
      `Como é a sua participação em feiras, seminários, congressos e afins na área da arquitetura e urbanismo?` == "Frequenta a maioria" ~ "Maioria",
      `Como é a sua participação em feiras, seminários, congressos e afins na área da arquitetura e urbanismo?` == "Não frequenta" ~ "Não frequenta",
      `Como é a sua participação em feiras, seminários, congressos e afins na área da arquitetura e urbanismo?` == "Frequenta todas" ~ "Todas",
    ) ,
    "Satisfação instituição ensino" = `Qual o seu nível de satisfação com a Instituição de Ensino onde concluiu sua formação como arquiteto e urbanista?`,
    "Acesso meios comunicação" = case_when(
      media_1 < 1.5 ~ 'Nunca',
      media_1 >= 1.5 & media_1 < 2.5 ~ 'Raramente',
      media_1 >= 2.5 & media_1 < 3.5 ~  'às vezes',
      media_1 >= 3.5 & media_1 < 4.5 ~  'Quase diariamente',
      media_1 >= 4.5 & media_1 <= 5 ~  'Diariamente'
    ),
    "Acesso dispositivos" = case_when(
      media_2 < 1.5 ~ 'Nunca',
      media_2 >= 1.5 & media_2 < 2.5 ~ 'Raramente',
      media_2 >= 2.5 & media_2 < 3.5 ~  'às vezes',
      media_2 >= 3.5 & media_2 < 4.5 ~  'Quase diariamente',
      media_2 >= 4.5 & media_2 <= 5 ~  'Diariamente'
    ),
    "Conhecimento informática" = `Como você classifica o seu conhecimento de Informática?`,
    "Conhecimento softwares AU" = case_when(
      media_3 < 1.5 ~ 'Não conheço',
      media_3 >= 1.5 & media_3 < 2.5 ~ 'Muito Ruim',
      media_3 >= 2.5 & media_3 < 3.5 ~  'Ruim',
      media_3 >= 3.5 & media_3 < 4.5 ~  'Bom',
      media_3 >= 4.5 & media_3 <= 5 ~  'Muito bom'
    ),
    "Usa redes sociais" = case_when(
      media_4 < 1.5 ~ "Não",
      media_4 >= 1.5 ~ "Sim"
    ),
    "Leitura" = `Quantos livros em média você costuma ler por ano?`,
    "Áreas informação" = `Assinale as áreas de informações que lhe despertam maior interesse.`,
    "Idiomas estrangeiros" = case_when(
      media_5 < 1.5 ~ 'Não falo nenhum idioma',
      media_5 >= 1.5 & media_5 < 2.5 ~ 'Básico',
      media_5 >= 2.5 & media_5 < 3.5 ~  'Intermediário',
      media_5 >= 3.5 & media_5 < 4.5 ~  'Avançado',
      media_5 >= 4.5 & media_5 <= 5 ~  'Fluente'
    ),
    "Raça/Cor" = `Dentre as opções abaixo, qual a cor ou a raça que você se identifica?`,
    "Gênero" = `Com qual gênero se identifica?`,
    "Deficiência" = case_when(
      `Possui deficiência` == "Nenhuma" ~ "Nenhuma",
      `Possui deficiência` == "Sim, sensorial" ~ "Sensorial",
      `Possui deficiência` == "Sim, física" ~ "Física",
      `Possui deficiência` == "Sim, mental" ~ "Mental",
      `Possui deficiência` == "Sim, intelectual" ~ "Intelectual"
    ),
    "Filiado" = case_when(
      `Você é filiado a alguma entidade profissional?` == "Não sou filiado a nenhuma entidade profissional" ~ "Não",
      `Você é filiado a alguma entidade profissional?` == "Outros" ~ "Outros",
      `Você é filiado a alguma entidade profissional?` == "IAB" ~ "IAB",
      `Você é filiado a alguma entidade profissional?` == "ASBEA" ~ "ASBEA",
      `Você é filiado a alguma entidade profissional?` == "ABAP" ~ "ABAP",
      `Você é filiado a alguma entidade profissional?` == "ABEA" ~ "ABEA",
      `Você é filiado a alguma entidade profissional?` == "FNA â€“ Sindicato de Arquitetos" ~ "FNA"
    ),
    "Obstáculos arquitetura" = case_when(
      `Na sua opinião, quais são os principais obstáculos que dificultam o exercício da profissão de arquiteto?` == "Valorização do Arquiteto e Urbanista pela Sociedade" ~ "Valorização",
      `Na sua opinião, quais são os principais obstáculos que dificultam o exercício da profissão de arquiteto?` == "Má remuneração" ~ "Má remuneração",
      `Na sua opinião, quais são os principais obstáculos que dificultam o exercício da profissão de arquiteto?` == "Sem acesso ao mercado de trabalho" ~ "Acesso mercado de trabalho"
    ),
    "Satisfação profissão" = case_when(
      media_6 < 1.5 ~ 'Totalmente Insatisfeito',
      media_6 >= 1.5 & media_6 < 2.5 ~ 'Parcialmente Insatisfeito',
      media_6 >= 2.5 & media_6 < 3.5 ~  'Nem satisfeito e nem insatisfeito',
      media_6 >= 3.5 & media_6 < 4.5 ~  'Parcialmente Satisfeito',
      media_6 >= 4.5 & media_6 <= 5 ~  'Totalmente Satisfeito'
    ),
    "Atuação 2 anos" = `Quais as suas áreas de atuação nos últimos 2 anos?`,
    "Visão mercado AU" = `Na sua opinião o mercado de trabalho para arquitetura e urbanismo está:`,
    "Visão nichos mercado AU" = case_when(
      media_7 < 1.5 ~ 'Sem resposta',
      media_7 >= 1.5 & media_7 < 2.5 ~ 'Em retração',
      media_7 >= 2.5 & media_7 < 3.5 ~  'Inalterado',
      media_7 >= 3.5 & media_7 <= 4 ~  'Em expansão'
    ),
    "Áreas inexploradas" = `Você considera que há outras áreas do mercado que ainda são inexploradas pelos arquitetos e urbanistas?`,
    "Dependentes financeiros" = case_when(
      `Possui dependentes financeiros (Selecionar quantos necessários)?` == "Filhos" ~ "Filhos",
      `Possui dependentes financeiros (Selecionar quantos necessários)?` == "Não possuo dependentes" ~ "Não possuo",
      `Possui dependentes financeiros (Selecionar quantos necessários)?` == "Outros" ~ "Outros",
      `Possui dependentes financeiros (Selecionar quantos necessários)?` == "Cônjuge e ex-cônjuges" ~ "Cônjuge e ex-cônjuge",
      `Possui dependentes financeiros (Selecionar quantos necessários)?` == "Idosos" ~ "Idosos"
    ),
    "Trabalha AU" = `Trabalha na área de Arquitetura e Urbanismo?`,
    "Pessoa Jurídica AU" = case_when(
      `Possui pessoa(s)jurídica(s) na área da arquitetura e urbanismo?` == "Não" ~ "Não",
      `Possui pessoa(s)jurídica(s) na área da arquitetura e urbanismo?` == "Sim, Uniprofissionais apenas com Arquitetos e Urbanistas" ~ "Uniprofissionais",
      `Possui pessoa(s)jurídica(s) na área da arquitetura e urbanismo?` == "Sim, Mista com Arquitetos e Urbanistas e outras profissõees" ~ "Mista"
    ),
    "Contribuição previdência" = case_when(
      `Contribui para a previdência?` == "Não" ~ "Não",
      `Contribui para a previdência?` == "Sim, privada" ~ "Privada",
      `Contribui para a previdência?` == "Sim, pública" ~ "Pública",
      `Contribui para a previdência?` == "Sim, pública Privada" ~ "Pública privada"
    ),
    "Grau escolaridade" = `Marque abaixo a opção de acordo com seu grau de escolaridade.`,
    "Outro curso superior completo" = `Além de arquitetura e urbanismo possui outro curso superior?`,
    "Cursando curso superior" = `Está cursando algum outro curso superior?`,
    "Pretende curso superior" = `Pretende fazer outro curso superior?`,
    "Sites CAU" = `Você frequenta os sites do Conselho de Arquitetura e Urbanismo?`,
    "Contratantes 2 anos" = `Quais tipos de contratantes você trabalhou nos últimos 2 anos?`,
    "Valor dimensionamento honorários" = case_when(
      `Qual o valor de referência que você usa para dimensionar os honorários para elaboração de projeto arquitetônico?` == "Outra" ~ "Outra",
      `Qual o valor de referência que você usa para dimensionar os honorários para elaboração de projeto arquitetônico?` == "Valos m²" ~ "Valos m²",
      `Qual o valor de referência que você usa para dimensionar os honorários para elaboração de projeto arquitetônico?` == "C.U.B." ~ "C.U.B.",
      `Qual o valor de referência que você usa para dimensionar os honorários para elaboração de projeto arquitetônico?` == "Tabela de honorários do CAU/BR" ~ "Tabela CAU/BR"
    ),
    "Trabalho semanal AU" = case_when(
      `Quantas horas por semana você trabalha com arquitetura e urbanismo?` == "Trabalho esporadicamente" ~ "Esporadicamente",
      `Quantas horas por semana você trabalha com arquitetura e urbanismo?` == "Até 10 horas" ~ "Até 10 horas",
      `Quantas horas por semana você trabalha com arquitetura e urbanismo?` == "De 10 a 20 horas" ~ "10 a 20 horas",
      `Quantas horas por semana você trabalha com arquitetura e urbanismo?` == "De 20 a 30 horas" ~ "20 a 30 horas",
      `Quantas horas por semana você trabalha com arquitetura e urbanismo?` == "De 30 a 40 horas" ~ "30 a 40 horas",
      `Quantas horas por semana você trabalha com arquitetura e urbanismo?` == "Mais de 40 horas" ~ "Mais de 40 horas",
      `Quantas horas por semana você trabalha com arquitetura e urbanismo?` == "Não trabalho com Arquitetura e Urbanismo" ~ "Não trabalho"
    ),
    "Trabalho semanal fora AU" = case_when(
      `Quantas horas por semana você trabalha com outra atividade fora da área da arquitetura e urbanismo?` == "Trabalho esporadicamente" ~ "Esporadicamente",
      `Quantas horas por semana você trabalha com outra atividade fora da área da arquitetura e urbanismo?` == "Até 10 horas" ~ "Até 10 horas",
      `Quantas horas por semana você trabalha com outra atividade fora da área da arquitetura e urbanismo?` == "De 10 a 20 horas" ~ "10 a 20 horas",
      `Quantas horas por semana você trabalha com outra atividade fora da área da arquitetura e urbanismo?` == "De 20 a 30 horas" ~ "20 a 30 horas",
      `Quantas horas por semana você trabalha com outra atividade fora da área da arquitetura e urbanismo?` == "De 30 a 40 horas" ~ "30 a 40 horas",
      `Quantas horas por semana você trabalha com outra atividade fora da área da arquitetura e urbanismo?` == "Mais de 40 horas" ~ "Mais de 40 horas",
      `Quantas horas por semana você trabalha com outra atividade fora da área da arquitetura e urbanismo?` == "Não trabalho com Arquitetura e Urbanismo" ~ "Não trabalho"
    ),
    "Acesso sites entidades AU" = case_when(
      `Você frequenta sites das entidades de Arquitetos e Urbanistas?` == "Não frequento" ~ "Não frequento",
      `Você frequenta sites das entidades de Arquitetos e Urbanistas?` == "Outros sites profissionais" ~ "Outros sites",
      `Você frequenta sites das entidades de Arquitetos e Urbanistas?` == "ASBEA" ~ "ASBEA",
      `Você frequenta sites das entidades de Arquitetos e Urbanistas?` == "IAB" ~ "IAB",
      `Você frequenta sites das entidades de Arquitetos e Urbanistas?` == "ABEA" ~ "ABEA",
      `Você frequenta sites das entidades de Arquitetos e Urbanistas?` == "FNA" ~ "FNA",
      `Você frequenta sites das entidades de Arquitetos e Urbanistas?` == "ABAP" ~ "ABAP",
    ),
    "Tipos projetos" = case_when(
      `Nos projetos arquitetônicos que realiza, você executa predominantemente:` == "Projeto Executivo" ~ "Executivo",
      `Nos projetos arquitetônicos que realiza, você executa predominantemente:` == "Projeto de aprovação ou básico" ~ "Aprovação ou básico",
      `Nos projetos arquitetônicos que realiza, você executa predominantemente:` == "Execução de obras" ~ "Execução obras",
      `Nos projetos arquitetônicos que realiza, você executa predominantemente:` == "Fiscalização direção de obras" ~ "Fiscalização direção obras",
      `Nos projetos arquitetônicos que realiza, você executa predominantemente:` == "Coordenação dos projetos complementares" ~ "Coordenação projetos complementares",
      `Nos projetos arquitetônicos que realiza, você executa predominantemente:` == "Autoria dos projetos complementares" ~ "Autoria projetos complementares"
    ),
    "Papel política" = case_when(
      `Que papel tem a política na sua vida?` == "Importante. Eventualmente discuto sobre política com meus colegas." ~ "Importante",
      `Que papel tem a política na sua vida?` == "Não me interesso, Não entende ou não gosto de política." ~ "Sem interesse, entendimento ou não gosta",
      `Que papel tem a política na sua vida?` == "Muito Importante. Procuro discutir sobre política com meus colegas." ~ "Muito Importante",
      `Que papel tem a política na sua vida?` == "Não me preocupo com política ou não costumo discutir sobre política" ~ "Não se preocupa ou costuma discutir"
    ),
    "Atuação docência" = case_when(
      `Você atua como docente na área de arquitetura e urbanismo?` == "Não" ~ "Não",
      `Você atua como docente na área de arquitetura e urbanismo?` == "Sim e também como profissional" ~ "Sim e como profissional",
      `Você atua como docente na área de arquitetura e urbanismo?` == "Sim com dedicação exclusiva" ~ "Sim exclusivamente"
    )
  ) 


## salvando o banco
saveRDS(banco_m, "banco/banco_m.rds")










