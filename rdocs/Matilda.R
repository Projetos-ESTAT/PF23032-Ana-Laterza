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

#lendo o banco ----
banco1 <- read_excel('banco/perguntasabertas-para-ESTAT.xlsx', sheet = 1)
banco2 <- read_excel('banco/perguntasabertas-para-ESTAT.xlsx', sheet = 2)
#textos sem uma classificação da ID ----
na_1 <- banco1 %>% filter(is.na(CLASSIFICACAO))
na_2 <- banco2 %>% filter(is.na(CLASSIFICACAO))

#manipulação str banco1 ----

#manipulação str banco2 ----

