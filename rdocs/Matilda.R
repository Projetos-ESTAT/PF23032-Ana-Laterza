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

# carregando pacotes para realizacao da nuvem de palavras
pacman::p_load(ggcorrplot, knitr,showtext, kableExtra, data.table, tidyr,SnowballC,
               wordcloud,tm,stringr)

#lendo o banco ----
banco1 <- read_excel('banco/perguntasabertas-para-ESTAT.xlsx', sheet = 1)
banco2 <- read_excel('banco/perguntasabertas-para-ESTAT.xlsx', sheet = 2)
#textos sem uma classificação da ID ----
na_1 <- banco1 %>% filter(is.na(CLASSIFICACAO))
na_2 <- banco2 %>% filter(is.na(CLASSIFICACAO))

#manipulação str banco1 ----

#manipulação str banco2 ----

# Função para pré-processar texto ----

preprocess_text <- function(text) {
  text <- tolower(text)
  text <- removePunctuation(text)
  text <- removeNumbers(text)
  text <- stripWhitespace(text)
  text <- removeWords(text, stopwords("pt"))  
  return(text)
}

# Pré-processando os textos
banco2$TEXTO <- sapply(banco2$TEXTO, preprocess_text)

# criando um corpus (palavra) agrupado por grupo de id
corpus_list <- split(banco2$TEXTO, banco2$CLASSIFICACAO)
corpus <- Corpus(VectorSource(corpus_list))

# criando uma matriz de documentos da coluna de texto do banco a partir do corpus
dtm <- DocumentTermMatrix(corpus)

# convertendo a matriz em um dataframe
text_df <- as.data.frame(as.matrix(dtm))
text_df <- text_df[,-c(1:5,1261)]
text_df$grupo <- rownames(text_df)

# calculando a frequência das palavras por grupo
palavras_frequencia <- text_df %>%
  group_by(grupo) %>%
  summarise_all(sum) %>%
  pivot_longer(cols = -grupo, names_to = "palavra", values_to = "frequencia") %>%
  arrange(grupo, desc(frequencia))

palavras_frequencia$grupo <- as.factor(palavras_frequencia$grupo)
levels(palavras_frequencia$grupo) <- c('1','2','3','norma')
# criando nuvens de palavras para cada grupo

#grupo1
grupo1 <- palavras_frequencia %>%
  filter(grupo == '1') %>%
  top_n(20,wt = frequencia)
grupo1 <- grupo1[-c(1:2,9,11,15,17,19),] #removendo as stopwords que não foram retiradas
grupo1_nuvem <- wordcloud(words = grupo1$palavra,scale = c(3,0.5), freq = grupo1$frequencia,colors = cores_estat, max.words = 50) # tá dando erro essa merda de nuvem
grupo1 <- grupo1[1:10,]
#grupo2
grupo2 <- palavras_frequencia %>%
  filter(grupo == '2') %>%
  top_n(20,wt = frequencia)
grupo2 <- grupo2[-c(1:2,8:9,15,17:18),] #removendo as stopwords que não foram retiradas
grupo2_nuvem <- wordcloud(words = grupo2$palavra,scale = c(3,.5), freq = grupo2$frequencia,colors = cores_estat) # tá dando erro essa merda de nuvem
grupo2 <- grupo2[1:10,]
#grupo 3
grupo3 <- palavras_frequencia %>%
  filter(grupo == '3') %>%
  top_n(20,wt = frequencia)
grupo3 <- grupo3[-c(2:3,7:8,10,16,17,19,21),] #removendo as stopwords que não foram retiradas
grupo3_nuvem <- wordcloud(words = grupo3$palavra,scale = c(3,.5), freq = grupo3$frequencia,colors = cores_estat) # tá dando erro essa merda de nuvem
grupo3 <- grupo3[1:10,]

#grupo norma
grupo_n <- palavras_frequencia %>%
  filter(grupo == 'norma') %>%
  top_n(20,wt = frequencia)
grupo_n <- grupo_n[-c(1:2,9,13,15,17,20),] #removendo as stopwords que não foram retiradas
grupo_n_nuvem <- wordcloud(words = grupo_n$palavra,scale = c(3,.5), freq = grupo_n$frequencia,colors = cores_estat) # tá dando erro essa merda de nuvem
grupo_n <- grupo_n[1:10,]
