library(readODS)
library(stringr)
library(data.table)
library(tidytext)
library(tidyverse)
library(readxl)
rm(list=ls())
#Lendo as Bases de dados do web scrapping t0
setwd('/home/user/Documentos/PNUD CNI/bases scraping')
x1 <- read.csv("Indeed_t0_consolidada_asct.csv", sep=',')
x2 <- read.csv("Infojobs_t0_consolidada_asct.csv", sep=",")
x3 <- read.csv("Site_Vagas_t0_consolidada_sact.csv", sep=",")
#x4 <- read.csv("vagasCertas2021_05_10_Yuan_t0_wide.csv", sep=",")

#lista de stopwords
sw <- read.delim('/home/user/Documentos/PNUD CNI/stopwords.txt', header = TRUE)   #lendo a lista de stopwords
sw$stopwords <- as.character(sw$stopwords)
sw$stopwords <- trimws(sw$stopwords, which = c("both"), whitespace = "[ \t\r\n]") #retirando espações em branco no inicio e final 
#função que retira stopwords de um texto
rm_words <- function(string, words) {
  stopifnot(is.character(string), is.character(words))
  spltted <- strsplit(string, " ", fixed = TRUE) # fixed = TRUE for speedup
  vapply(spltted, function(x) paste(x[!tolower(x) %in% words], collapse = " "), character(1))
}

#base consolidada
colnames(x1)[11] <- 'conteudo'
x1$beneficios <- "-"
x2$beneficios <- "-"
x1$jornada <- "-"
x3$jornada <- "-"
x3$regime <- "-"
#x3 <- x3 %>% select(-beneficios)
#coluna beneficios foi removida de x3
x2 <- x2[,colnames(x1)]
x3 <- x3[,colnames(x1)]

base_to <- rbind(x1,x2,x3)
base_to2 <- base_to %>% rename(nome_vaga=profissao) %>%
  mutate(nome_vaga=trimws(nome_vaga, which = c("both"), whitespace = "[ \t\r\n]")) %>%
                       filter(str_detect(nome_vaga, "(?i)logistic|(?i)logístic")) %>% #filtrando para as vagas de logistica
                       #abaixo, arrumando a coluna conteudo
                       mutate(conteudo=iconv(conteudo,to="ASCII//TRANSLIT")) %>%   #retira acentos
                       mutate(conteudo=str_replace_all(conteudo,"[/''?.,;:()]", "")) %>% #retira pontuações e outros simbolos
                       mutate(conteudo_ssw=rm_words(conteudo,sw$stopwords)) %>%  #retirar stopwords
                       mutate(conteudo_ssw=trimws(conteudo_ssw, which = c("both"), whitespace = "[ \t\r\n]")) %>% #retira espaçoes em branco no inicio e fim
                       mutate(conteudo_ssw=tolower(conteudo_ssw))     #transformando todas as letras em minusculas

#Preparando a base do painel para comparação
my_data <- read_excel("Base_MSP_11jun2021.xlsx", sheet='Impactos_ocupacionais')
colnames(my_data)[c(2,7)] <- c('Nome_Setor','Desc.Item')
mydata2 <- my_data %>% filter(Nome_Setor=='Logística') %>% select(c(1:3,6,7,17)) %>% rename('Item'='...6','Palavras_chave'="Palavras_chaves") %>% 
           mutate(Desc.Item=trimws(Desc.Item, which='both')) %>%
           filter(is.na(Palavras_chave)==FALSE)
lista <- as.data.frame(unique(mydata2$Desc.Item))
lista2 <- lista %>% mutate(ID=row_number()) %>% rename("Desc.Item"="unique(mydata2$Desc.Item)")
#variavel de identificação, e arrumando as palabras chave
mydata3 <- mydata2 %>% left_join(lista2, by='Desc.Item') %>% mutate(Palavras_chave=gsub("/",",",Palavras_chave),Palavras_chave=gsub(";",",",Palavras_chave)) %>%
                  mutate(Palavras_chave=str_replace_all(Palavras_chave,"[/''?.()]", "")) %>%   #aqui, retirando pontuações e termos estranhos
                  separate(Palavras_chave, c("PC1", "PC2", "PC3", "PC4", "PC5","PC6","PC7","PC8"), c(","),remove=FALSE) %>%
                  gather(a,termo,PC1:PC7) %>% filter(is.na(termo)==FALSE) %>% mutate(termo=iconv(termo,to="ASCII//TRANSLIT")) %>%  #Retirar acentos
                  select(-a) %>% 
                  mutate(termo_ssw=rm_words(termo,sw$stopwords))%>%     #retirar stopwords
                  mutate(termo_ssw=trimws(termo_ssw, which = c("both"), whitespace = "[ \t\r\n]")) %>%   #retirar espaçoes em branco
                  mutate(termo_ssw=tolower(termo_ssw)) #transformando as letras em minusculas

#PROXIMO PASSO: CONTAR NA BASE VAGAS AS PALAVRAS DA BASE PAINEL DE ESPECIALISTAS
contagem <- mydata3 %>% select("Palavras_chave","ID","termo","termo_ssw") %>% arrange(ID) %>% mutate(freq_ws=NA)
for (i in 1:251){
contagem$freq_ws[i] <- length(grep(contagem$termo_ssw[i], base_to2$conteudo_ssw))
}


%>% unnest_tokens(word, termo) %>% mutate(word=trimws(word, which = c("both"), whitespace = "[ \t\r\n]"))


tidy_base <- mydata3 %>%
  unnest_tokens(word, Palavras_chave)

#Conta palavras que aparecem bastante juntas em uma mesma vaga de emprego -pode ser util para saber o que acompanha certas palavras
#correlação entre palavras em uma mesma vaa também pode ser util- fazer cluster de palavras
#TAMBEM PODE SER IMPORTANTE uma análise dos termos mais específicos nas vagas de cada profissão. Ex: em tecnico logística, tecnico administrativo, etc
#indicador tf_idf (p/ especificidade de palavras
#mudança de frequencia de certas termos ao longo do tempo)
tidy_base2 <- y %>%
  unnest_tokens(word, Conteúdo)
section_words <- tidy_base %>%
  filter(TAG == "Conteudo") %>%
  select("ID", "word")
library(widyr)
# count words co-occuring within sections
word_pairs <- section_words %>%
  pairwise_count(word, ID, sort = TRUE)
word_pairs
