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

# preprocess_text <- function(text) {
#   text <- tolower(text)
#   text <- removePunctuation(text)
#   text <- removeNumbers(text)
#   text <- stripWhitespace(text)
#   text <- removeWords(text, filtro)  
#   text <- str_replace_all(text, "\\b(\\w+)s\\b", "\\1")
#   return(text)
# }

# Pré-processando os textos
banco2$TEXTO <- tolower(banco2$TEXTO)
banco2$TEXTO <- removePunctuation(banco2$TEXTO)
banco2$TEXTO <- stripWhitespace(banco2$TEXTO)
banco2$TEXTO <- str_replace_all(banco2$TEXTO, "\\b(\\w+)s\\b", "\\1")
# criando um corpus para geral 
lista <- list()
for (i in 1:nrow(banco2)){
  lista[[i]] <- banco2[i,3]
}
vetor <- unlist(lista)
docs <- Corpus(VectorSource(vetor))

docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

filtro <- c('que','para','com','isso','tem','por','uma','pra','esse','mais',
            'dos','das','essa','tá','nas','nem','sem','aos','sobre','aí','pois',
            'este','esse','dos','ela','pra', 'está', 'mas',"vamos","vai",
            "estão","nossa","foi","nosso","nos","aqui","ainda","meu","ter",
            "porque","nossos","são","vão","você","sua","seu","acima","seja",
            "temos","será","sera","mesmo","pelo","só","so","então","entao",
            "umas","como","dele","fazendo","galera","alguém","alguem","assim",
            "número","numero","qualquer","ser","ficar","fazer","ja","já","nossas",
            "manter","estou","cara","nesse","olha","sair","passar","esses","estas",
            "dar","pela","esta","apenas","pro","souza","silva","chegar","dessa",
            "roberto","outra","sendo","estava","gilson","josé","durante","marcus",
            "marcos","desse","vou","tirar","era","teve","vocês","voces","ontem",
            "também","tbm","tambem","atraves","através","única","unica","costa",
            "consegue","as","às","ás","vem","vêm","null","não","ele","la","lá",
            "sim","boa","estamos","agora","hoje","dia","muito","quem","até",
            "pode","bom","nome","quando","coisa") # adicionar mais conforme necessidade

#rm(df2,docs,dtm,lista,matrix,filtro,i,vetor,words)
#gc()

# rode este bloco apenas se desejar filtrar palavras específicas
df <- df |>
  filter(!(word %in% filtro)) |> # Aplicando filtros
  filter(freq >9)

set.seed(150167636) 

wordcloud(words = df$word, freq = df$freq, min.freq = 2,
          max.words=200, random.order=FALSE, rot.per=0.35, # Ajuste o parâmetro max.words conforme necessidade. O máximo é 200.
          colors=c("#CA1D1F", "#F55D1C", "#F55751", "#086C75"))
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

palavras_frequencia <- palavras_frequencia |> filter(!(palavra %in% filtro))

palavras_frequencia$grupo <- as.factor(palavras_frequencia$grupo)
levels(palavras_frequencia$grupo) <- c('1','2','3','norma')
# criando nuvens de palavras para cada grupo

#grupo1
grupo1 <- palavras_frequencia %>%
  filter(grupo == '1') %>%
  top_n(20,wt = frequencia)
grupo1 <- grupo1[-c(2,7),] #removendo as stopwords que não foram retiradas
grupo1[4,3] <- 1995
grupo1 <- grupo1 %>% arrange(desc(frequencia))
wordcloud(words = grupo1$palavra,scale = c(3,0.5), freq = grupo1$frequencia,colors = c("#CA1D1F", "#F55D1C", "#F55751", "#086C75")) 
grupo1 <- grupo1[1:10,] #top 10
#grupo2
grupo2 <- palavras_frequencia %>%
  filter(grupo == '2') %>%
  top_n(20,wt = frequencia)
grupo2 <- grupo2[-c(2,7),]
grupo2[3,3] <- 323 + 274
grupo2 <- grupo2 %>% arrange(desc(frequencia))
wordcloud(words = grupo2$palavra,scale = c(3,.5), freq = grupo2$frequencia,colors = c("#CA1D1F", "#F55D1C", "#F55751", "#086C75")) # tá dando erro essa merda de nuvem
grupo2 <- grupo2[1:10,] #top10
#grupo 3
grupo3 <- palavras_frequencia %>%
  filter(grupo == '3') %>%
  top_n(20,wt = frequencia)
grupo3 <- grupo3[1:20,]#removendo as stopwords que não foram retiradas
grupo3 <- grupo3[-c(2,6,9,15,20),]
wordcloud(words = grupo3$palavra,scale = c(3,.5), freq = grupo3$frequencia,colors = c("#CA1D1F", "#F55D1C", "#F55751", "#086C75")) # tá dando erro essa merda de nuvem
grupo3 <- grupo3[1:10,]#top10

#grupo norma
grupo_n <- palavras_frequencia %>%
  filter(grupo == 'norma') %>%
  top_n(20,wt = frequencia)
grupo_n <- grupo_n[-c(2,10),] #removendo as stopwords que não foram retiradas
grupo_n <- grupo_n[-c(3,4,10,12),] 
grupo_n[3,3] <- 869
grupo_n[10,3] <- 491
grupo_n <- grupo_n %>% arrange(desc(frequencia))
wordcloud(words = grupo_n$palavra,scale = c(3,.5), freq = grupo_n$frequencia,colors = c("#CA1D1F", "#F55D1C", "#F55751", "#086C75")) # tá dando erro essa merda de nuvem
grupo_n <- grupo_n[1:10,]#top10
