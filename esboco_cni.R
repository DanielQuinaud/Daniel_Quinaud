library(readODS)
library(stringr)
library(data.table)
library(tidytext)
library(tidyverse)
library(readxl)
rm(list=ls())
#Bases de dados do web scrapping t0
setwd('/home/user/Documentos/PNUD CNI/bases scraping')
x <- read.csv("consolidada_1.csv", sep=';')

y <- read.csv("consolidada_2.csv", sep=";")

#base consolidada
base_to <- rbind(x,y)


#Preparando a base do painel para comparação
my_data <- read_excel("Base_MSP_logistica.xlsx", sheet='Impactos_ocupacionais')
colnames(my_data)[c(7,12)] <- c('Desc.Item',"Palavras_chave")
mydata2 <- my_data %>% select(c(1:3,6,7,12)) %>% rename('Item'='...6') %>% mutate(Desc.Item=trimws(Desc.Item, which='both')) %>%
          filter(is.na(Palavras_chave)==FALSE)
lista <- as.data.frame(unique(mydata2$Desc.Item))
lista2 <- lista %>% mutate(ID=row_number()) %>% rename("Desc.Item"="unique(mydata2$Desc.Item)")

mydata3 <- mydata2 %>% left_join(lista2, by='Desc.Item') %>% mutate(Palavras_chave=gsub("/",",",Palavras_chave),Palavras_chave=gsub(";",",",Palavras_chave)) %>%
  mutate(Palavras_chave=str_replace_all(Palavras_chave,"[/''?.()]", "")) %>%
  separate(Palavras_chave, c("PC1", "PC2", "PC3", "PC4", "PC5","PC6","PC7"), c(","),remove=FALSE) 
#Retirar acentos e pontuações

mydata4 <- mydata3 %>% gather(a,termo,PC1:PC7) %>% filter(is.na(termo)==FALSE) %>% mutate(termo=iconv(mydata4$termo,to="ASCII//TRANSLIT"))






##removendo stopwords da base
sw <- read.delim('/home/user/Documentos/PNUD CNI/stopwords.txt', header = TRUE)
sw$stopwords <- as.character(sw$stopwords)
sw$stopwords <- trimws(sw$stopwords, which = c("both"), whitespace = "[ \t\r\n]")

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
