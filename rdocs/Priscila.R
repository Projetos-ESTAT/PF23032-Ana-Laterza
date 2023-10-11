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
library(tidyverse)
library(stringr)
library(readxl)

banco <- read_excel("C:/Users/prima/Downloads/banco.xlsx", 
                        sheet = "respostas")

cores_personalizadas <- c( "#CA1D1F", "#F55D1C", "#FDC500", "#F55751", "#086C75", "#17B2A7", "#69B2A7", "#AB324A")

theme_ble <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "white", size = 10),
      axis.title.x = ggplot2::element_text(colour = "white", size = 10),
      axis.text = ggplot2::element_text(colour = "white", size = 10),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "white"),
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
   

percent <- function(absolute, digits = 2) {
  return(round(100 * absolute / sum(absolute), digits))
}




#sites conselho

banco <- banco %>%
  mutate(across(everything(), ~ ifelse(. == "0.0", NA, .)))


conse <- banco %>%
  filter(!is.na(`Você frequenta os sites do Conselho de Arquitetura e Urbanismo?`)) %>%
  group_by(`Você frequenta os sites do Conselho de Arquitetura e Urbanismo?`) %>%
  summarise(Freq = n()) %>%
  mutate(Prop = round(100 * (Freq / sum(Freq)), 2)) %>%
  arrange(desc(`Você frequenta os sites do Conselho de Arquitetura e Urbanismo?`)) %>%
  mutate(posicao = cumsum(Prop) - 0.5 * Prop,
         ymax = cumsum(Prop),
         ymin = c(0, head(ymax, n=-1)))

conse <- conse %>% 
  group_by(`Você frequenta os sites do Conselho de Arquitetura e Urbanismo?`) %>%
  summarise(Freq = n()) %>%
  mutate(Prop = round(100*(Freq/sum(Freq)), 2)) %>%
  arrange(desc(`Você frequenta os sites do Conselho de Arquitetura e Urbanismo?`)) %>%
  mutate(posicao = cumsum(Prop) - 0.5*Prop)

ggplot(conse) +
  aes(x = factor(""), y = Prop , fill = factor(`Você frequenta os sites do Conselho de Arquitetura e Urbanismo?`)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(
    aes(x = 1.8, y = posicao, label = paste0(Prop, "%")),
    color = "black"
  ) +
  theme_void() +
  theme(legend.position = "top",
        panel.background = element_rect(fill = 'gray', color = 'gray'))+
  ##scale_y_continuous(breaks = c(0,round(12269/4),round(12269/2),round(12269*3/4),12269),limits=c(0,12269)) +
  scale_fill_manual(values = cores_personalizadas, name = "Frequência em sites do Conselho de A/U")

caminho_download <- "C:/Users/prima/Downloads"

# Nome do arquivo de saída
nome_arquivo <- "sitesconse.pdf"

# Caminho completo para o arquivo de saída
caminho_completo <- file.path(caminho_download, nome_arquivo)

# Salvar o gráfico no caminho especificado
ggsave(filename = caminho_completo, width = 158, height = 93, units = "mm")



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
    label = str_c(freq, "\n(", n, ")") %>% str_squish())
  

ggplot(sitentidades) +
  aes(
    x = fct_reorder(`Você frequenta sites das entidades de Arquitetos e Urbanistas?`, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#CA1D1F", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "Frequência em sites das entidades", y = "Frequência") +scale_y_continuous(
    breaks = seq(0, 25600, 6400)
  )+
  theme(
    panel.background = element_rect(fill = 'gray90'),
    axis.line.x = element_line(colour = "black"),  # Personaliza a cor do eixo x
    axis.line.y = element_line(colour = "black"))
  

nome_arquivo <- "site_entidade.pdf"

# Caminho completo para o arquivo de saída
caminho_completo <- file.path(caminho_download, nome_arquivo)

# Salvar o gráfico no caminho especificado
ggsave(filename = caminho_completo, width = 158, height = 93, units = "mm")

ggsave("site_entidades.pdf", width = 158, height = 93, units = "mm")


#Áreas de informação de despertam mais interesse 


interesse <- banco %>%
  filter(!is.na(`Assinale as áreas de informações que lhe despertam maior interesse.`)) %>%
  count(`Assinale as áreas de informações que lhe despertam maior interesse.`) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(freq, "\n(", n, ")") %>% str_squish())

label_perso <- c("26,31% \n(10416)","14,94% \n(5916)","6,65% \n(2633)","8,35% \n(3307)","11,53% \n(4566)","12,81% \n(5073)","8,05% \n(3188)","8,31% \n(3288)","3,03% \n(1201)")


ggplot(interesse) +
  aes(
    x = fct_reorder(`Assinale as áreas de informações que lhe despertam maior interesse.`, n, .desc = T),
    y = n,
    label = label_perso
  ) +
  geom_bar(stat = "identity", fill = "#CA1D1F", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5,
    size = 3
  ) +
  scale_x_discrete(
    labels = c("Cultura \ne Lazer","Economia \ne Negócios", "Notícias \nLocais","Notícias \nInternacionais","Informática", "Outros" , "Notícias \nPolíticas","Esportes", "Veículos"),
    expand = c(0, 0)  # Ajuste os valores de expand conforme necessário
  ) +
  labs(x = "Áreas de interesse", y = "Frequência") +
  scale_y_continuous(
    breaks = seq(0, 12000, 3000)
  ) +
  theme(
    panel.background = element_rect(fill = 'gray90'),
    axis.line.x = element_line(colour = "black"),  # Personaliza a cor do eixo x
    axis.line.y = element_line(colour = "black"))+
  coord_cartesian(ylim = c(0, 12000))



nome_arquivo <- "areas_interesse.pdf"

# Caminho completo para o arquivo de saída
caminho_completo <- file.path(caminho_download, nome_arquivo)

# Salvar o gráfico no caminho especificado
ggsave(filename = caminho_completo, width = 158, height = 93, units = "mm")

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
    label = str_c(freq, "\n(", n, ")") %>% str_squish())
  


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
  geom_bar(stat = "identity", fill = "#CA1D1F", width = 0.7) +
  geom_text(
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3
  ) +
  labs(x = "Papel da política na sua vida", y = "Frequência") +
  scale_x_discrete(labels= c("Importante","Muito\nimportante", "Não me interesso\nNão discuto", "Não me\npreocupo", "Outros"))+
  theme(
    panel.background = element_rect(fill = 'gray90'),
    axis.line.x = element_line(colour = "black"),  # Personaliza a cor do eixo x
    axis.line.y = element_line(colour = "black"))


nome_arquivo <- "política.pdf"

# Caminho completo para o arquivo de saída
caminho_completo <- file.path(caminho_download, nome_arquivo)

# Salvar o gráfico no caminho especificado
ggsave(filename = caminho_completo, width = 158, height = 93, units = "mm")


#nível de satisfação em relação à


teste <- banco[, c(
  "Status Social da profissão de arquiteto e urbanista",
  "Exercício da Profissão de Arquitetura e Urbanismo",
  "Rendimentos mensais da profissão de Arquitetura e Urbanismo",
  "Tecnologias de software disponíveis a profissão de Arquitetura e Urbanismo"
)]


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
t <- sum(is.na(teste$Respostas))


ggplot(teste, aes(x = Satisfação, fill = Respostas))+
  geom_bar(stat = "count", position = "fill") +
  scale_fill_manual(values = cores_personalizadas, name = "Respostas")+
  labs(x = "Opções", y = "Frequência")+
  scale_x_discrete(labels= c("Tecnologias \nde software \ndisponíveis","Status social\n da profissão", "Rendimentos\nmensais", "Exercícios da\n profissão"))+
  theme(axis.line = element_line(color = "black"))

ggsave("satisfacao.pdf", width = 158, height = 93, units = "mm")

nome_arquivo <- "satisfacao.pdf"

# Caminho completo para o arquivo de saída
caminho_completo <- file.path(caminho_download, nome_arquivo)

# Salvar o gráfico no caminho especificado
ggsave(filename = caminho_completo, width = 158, height = 93, units = "mm")




somas <- teste %>%
  count(Satisfação, Respostas)

tec <- somas$n

# Em seguida, selecione as linhas 6, 7, 8, 9 e 10 da coluna
amostra <- tec[16:20]

# Agora, você pode somar os valores na amostra
soma <- sum(amostra)

t <- ((amostra/soma)*100)


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

t <- sum(is.na(comp$Respostas))

comp1 <- comp %>%
  filter(!is.na(Respostas)) %>%
  count(Uso) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(freq, " (", n, ")") %>% str_squish())

ggplot(comp, aes(x = Uso, fill = Respostas))+
  geom_bar(stat = "count", position = "fill") +
  scale_fill_manual(values = cores_personalizadas, name = "Respostas")+
  labs(x = "Uso de aparelhos tecnológicos", y = "Frequência")+
  scale_x_discrete(labels= c("Celular \nsimples","Computadores \n(Desktops)", "Notebooks", "Outros \ndispositivos","Smartphones","Tablets"))+
  theme(axis.line = element_line(color = "black"))


comp$Respostas <- gsub("Não tenho acesso a outros dispositivos", "Não uso outros", comp$Respostas)

ggsave("uso_comp.pdf", width = 158, height = 93, units = "mm")

nome_arquivo <- "uso_comp.pdf"

caminho_completo <- file.path(caminho_download, nome_arquivo)

ggsave(filename = caminho_completo, width = 158, height = 93, units = "mm")

somas <- comp %>%
  count(Uso, Respostas)


tec <- somas$n

# Em seguida, selecione as linhas 6, 7, 8, 9 e 10 da coluna
amostra <- tec[16:20]

# Agora, você pode somar os valores na amostra
soma <- sum(amostra)

t <- ((amostra/soma)*100)





#LIVROS


livros <- banco %>%
  filter(!is.na(`Quantos livros em média você costuma ler por ano?`)) %>%
  count(`Quantos livros em média você costuma ler por ano?`) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(freq, " (", n, ")") %>% str_squish()
  )

livros$`Quantos livros em média você costuma ler por ano?` <- gsub("Um livro", "1 livro", livros$`Quantos livros em média você costuma ler por ano?`)
livros$`Quantos livros em média você costuma ler por ano?` <- gsub("Mais de 30 livros", "Mais de 30", livros$`Quantos livros em média você costuma ler por ano?`)


ordem_desejada <- c("Nenhum", "1 livro", "6 a 10 livros","11 a 15 livros", "16 a 20 livros", "21 a 30 livros", "Mais de 30")


livros$Ordem <- factor(livros$`Quantos livros em média você costuma ler por ano?`, levels = ordem_desejada)



ggplot(livros) +
  aes(
    x = Ordem,
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#CA1D1F", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "Quantidade de livros", y = "Frequência") +
  scale_y_continuous(
    breaks = seq(0, 18301, 4575)
  )+ theme(
    panel.background = element_rect(fill = 'gray90'),
    axis.line.x = element_line(colour = "black"),  # Personaliza a cor do eixo x
    axis.line.y = element_line(colour = "black"))

  
nome_arquivo <- "livros.pdf"

caminho_completo <- file.path(caminho_download, nome_arquivo)

ggsave(filename = caminho_completo, width = 158, height = 93, units = "mm")

#filiação política
filiado <- banco %>%
  filter(!is.na(`Você é filiado a alguma entidade profissional?`)) %>%
  count(`Você é filiado a alguma entidade profissional?`) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(freq, "\n(", n, ")") %>% str_squish())



filiado$`Você é filiado a alguma entidade profissional?` <- gsub("Não sou filiado a nenhuma entidade profissional", "Não sou filiado", filiado$`Você é filiado a alguma entidade profissional?`)
filiado$`Você é filiado a alguma entidade profissional?` <- gsub("FNA â€“ Sindicato de Arquitetos", "FNA", filiado$`Você é filiado a alguma entidade profissional?`)

ordem_desejada <- c("Não sou filiado", "Outros", "IAB","FNA", "ASBEA", "ABEA", "ABAP")

# Crie uma variável categórica com a ordem desejada
filiado$Ordem <- factor(filiado$filiacaoprof, levels = ordem_desejada)



filiado <- filiado %>%
  rename(`filiacaoprof` = `Você é filiado a alguma entidade profissional?`)


ggplot(filiado) +
  aes(
    x = Ordem,
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#CA1D1F", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5,
    size = 3
  ) +
  scale_x_discrete(
   expand = c(0.12, 0)  # Ajuste os valores de expand conforme necessário
  ) +
  labs(x = "Entidades profissionais", y = "Frequência") +
  scale_y_continuous(
    breaks = seq(0, 32200, 8050)
  ) +
  theme(
    panel.background = element_rect(fill = 'gray90'),
    axis.line.x = element_line(colour = "black"),  # Personaliza a cor do eixo x
    axis.line.y = element_line(colour = "black"))+
  coord_cartesian(ylim = c(0, 32200))

nome_arquivo <- "filiado.pdf"

caminho_completo <- file.path(caminho_download, nome_arquivo)

ggsave(filename = caminho_completo, width = 158, height = 93, units = "mm")



 #obstáculos na profissão

obst <- banco %>%
  filter(!is.na(`Na sua opinião, quais são os principais obstáculos que dificultam o exercício da profissão de arquiteto?`)) %>%
  group_by(`Na sua opinião, quais são os principais obstáculos que dificultam o exercício da profissão de arquiteto?`) %>%
  summarise(Freq = n()) %>%
  mutate(Prop = round(100 * (Freq / sum(Freq)), 2)) %>%
  arrange(desc(`Na sua opinião, quais são os principais obstáculos que dificultam o exercício da profissão de arquiteto?`)) %>%
  mutate(
    freq = paste0(  Prop,"%"," (",Freq, ")"),
    label = paste0(Freq, " (", Prop, ")"),
     posicao = cumsum(Prop) - 0.5*Prop
  )


obst <- obst %>% 
  group_by(`Na sua opinião, quais são os principais obstáculos que dificultam o exercício da profissão de arquiteto?`) %>%
  summarise(Freq = n()) %>%
  mutate(Prop = round(100*(Freq/sum(Freq)), 2)) %>%
  arrange(desc(`Na sua opinião, quais são os principais obstáculos que dificultam o exercício da profissão de arquiteto?`)) %>%
  mutate(posicao = cumsum(Prop) - 0.5*Prop)


obst$`Na sua opinião, quais são os principais obstáculos que dificultam o exercício da profissão de arquiteto?` <- gsub("Valorização do Arquiteto e Urbanista pela Sociedade", "Valorização da profissão", obst$`Na sua opinião, quais são os principais obstáculos que dificultam o exercício da profissão de arquiteto?`)
obst$`Na sua opinião, quais são os principais obstáculos que dificultam o exercício da profissão de arquiteto?` <- gsub("Sem acesso ao mercado de trabalho", "Sem acesso ao mercado", obst$`Na sua opinião, quais são os principais obstáculos que dificultam o exercício da profissão de arquiteto?`)




ggplot(obst) +
  aes(x = factor(""), y = Prop , fill = factor(`Na sua opinião, quais são os principais obstáculos que dificultam o exercício da profissão de arquiteto?`)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(
    aes(x = 1.3, y = posicao, label = freq),
    color = "black"
  ) +
  theme_void() +
  theme(legend.position = "top",
        panel.background = element_rect(fill = 'gray', color = 'gray'))+
  scale_fill_manual(values = cores_personalizadas, name = "Obstáculos")

# Nome do arquivo de saída
nome_arquivo <- "obstaculos.pdf"

caminho_completo <- file.path(caminho_download, nome_arquivo)

# Salvar o gráfico no caminho especificado
ggsave(filename = caminho_completo, width = 158, height = 93, units = "mm")

    





    
    #barras empilhadas de meios de comunicação
inf <- banco[, c(
      "Publicações acadêmicas de Arquitetura",
      "Revistas de Arquitetura",
      "Livros Técnicos em Arquitetura e Urbanismo",
      "Sites de Arquitetura e Urbanismo",
      "Jornais",
      "Revistas",
      "TV aberta",
      "TV a Cabo",
      "Internet",
      "Livros",
      "Rádio AM/FM",
      "Outros meios de comunicação?")]
    
    inf <- inf %>%
      rename(`Outros meios` = `Outros meios de comunicação?`)
    
    inf <- inf %>%
      rename(`Sites de A/U` = `Sites de Arquitetura e Urbanismo`,
             `Livros técnicos`= `Livros Técnicos em Arquitetura e Urbanismo`,
             `Revistas de A/U` = `Revistas de Arquitetura`,
             `Publicações acadêmicas` = `Publicações acadêmicas de Arquitetura`)
    
    
    
    inf <- inf %>%
      pivot_longer(
        cols = c( "Publicações acadêmicas",
                  "Revistas de A/U",
                  "Livros técnicos",
                  "Sites de A/U",
                  "Jornais",
                  "Revistas",
                  "TV aberta",
                  "TV a Cabo",
                  "Internet",
                  "Livros",
                  "Rádio AM/FM",
                  "Outros meios"),
        names_to = "meios",
        values_to = "Respostas")
    
    
    inf <- inf %>%
      filter(!is.na(Respostas))
    t <- sum(is.na(inf$Respostas))
    
    inf1 <- inf %>%
      filter(!is.na(Respostas)) %>%
      count(meios) %>%
      mutate(
        freq = n %>% percent(),
      ) %>%
      mutate(
        freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
        label = str_c(freq, " (", n, ")") %>% str_squish())
    
  categorias_frequencia <- c("Diariamente", "Quase diariamente", "Às vezes", "Raramente", "Não uso outros","Nunca")
    
somas <- inf %>%
    filter(meios == "Revistas") %>%
    count(meios, Respostas)

soma <- sum(somas$n)



    ggplot(inf, aes(x = meios, fill = Respostas))+
      geom_bar(stat = "count", position = "fill") +
      scale_fill_manual(values = cores_personalizadas, name = "Respostas")+
      labs(x = "Uso de aparelhos tecnológicos", y = "Frequência")+
      scale_x_discrete(labels= c("Internet","Jornais", "Livros", "Livros técnicos","Outros meios","Publicações \nacadêmicas","Rádio AM/FM","Revistas","Revistas de A/U","Sites de A/U","TV a Cabo","TV aberta"))+
      coord_flip()+
      theme(axis.line = element_line(color = "black"))
    
    
    
    inf$Respostas <- gsub("Não tenho acesso a outros meios de comunicação.", "Não uso outros", inf$Respostas)
    
    nome_arquivo <- "inf.pdf"
    
    caminho_completo <- file.path(caminho_download, nome_arquivo)
    
    ggsave(filename = caminho_completo, width = 158, height = 93, units = "mm")
    
    
    
    
#redes 
    
    
    redes <- banco[, c(
      "Facebook",
      "Twitter",
      "Linkedin",
      "Instagram",
      "Outras redes"
      )]
    
    
    redes <- redes %>%
      pivot_longer(
        cols = c( "Facebook",
                  "Twitter",
                  "Linkedin",
                  "Instagram",
                  "Outras redes"),
        names_to = "meios",
        values_to = "Respostas")
    
    
    t <- sum(is.na(redes$Respostas))
    
    redes <- redes %>%
      filter(!is.na(Respostas))
  
    ggplot(redes, aes(x = meios, fill = Respostas))+
      geom_bar(stat = "count", position = "fill") +
      scale_fill_manual(values = cores_personalizadas, name = "Respostas")+
      labs(x = "Uso de redes sociais", y = "Frequência")+
      theme(axis.line = element_line(color = "black"))
    
    
    
    
  redes1 <- redes %>%
      filter(!is.na(Respostas)) %>%
      count(meios) %>%
      mutate(
        freq = n %>% percent(),
      ) %>%
      mutate(
        freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
        label = str_c(freq, " (", n, ")") %>% str_squish())
  
  somas <- redes %>%
    filter(meios == "Twitter") %>%
    count(meios, Respostas)
  
  
  nome_arquivo <- "redes.pdf"
  
  caminho_completo <- file.path(caminho_download, nome_arquivo)
  
  ggsave(filename = caminho_completo, width = 158, height = 93, units = "mm")
    
