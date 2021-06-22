library(readODS)
library(stringr)
library(data.table)
library(tidytext)
library(tidyverse)
library(readxl)
rm(list=ls())
#Lendo as Bases de dados do web scrapping t0
setwd('/home/user/Documentos/PNUD CNI/bases scraping')
x1 <- read.csv("Indeed_t0.csv", sep=',', stringsAsFactors = FALSE)
x2 <- read.csv("Infojobs_t0.csv", sep=",", stringsAsFactors = FALSE)
x3 <- read.csv("SiteVagas_t0.csv", sep=",", stringsAsFactors = FALSE)
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
x2$X3_situacao.da.vaga <- "-"
x2$X3_data.do.request <- "-"

#x3 <- x3 %>% select(-beneficios)
#coluna beneficios foi removida de x3
x2 <- x2[,colnames(x1)]
x3 <- x3[,colnames(x1)]

base_to <- rbind(x1,x2,x3)
base_to$site <- qdapRegex::ex_between(base_to$url, "https://", ".com")
base_to <- base_to[,c(1,22,2:21)]
base_to$site <- gsub("www.","",base_to$site,)
base_to$site <- gsub("br.","",base_to$site,)

base_total <- base_to %>% rename(nome_vaga=profissao) %>%
  mutate(nome_vaga=trimws(nome_vaga, which = c("both"), whitespace = "[ \t\r\n]")) %>%
  mutate(nome_vaga=iconv(nome_vaga,to="ASCII//TRANSLIT")) %>%
  mutate(nome_vaga=tolower(nome_vaga)) %>%                   
                       #abaixo, arrumando a coluna conteudo
                       mutate(conteudo=iconv(conteudo,to="ASCII//TRANSLIT")) %>%   #retira acentos
                       mutate(conteudo=str_replace_all(conteudo,"[/''?.,;:()]", "")) %>% #retira pontuações e outros simbolos
                       mutate(conteudo_ssw=rm_words(conteudo,sw$stopwords)) %>%  #retirar stopwords
                       mutate(conteudo_ssw=trimws(conteudo_ssw, which = c("both"), whitespace = "[ \t\r\n]")) %>% #retira espaçoes em branco no inicio e fim
                       mutate(conteudo_ssw=tolower(conteudo_ssw))     #transformando todas as letras em minusculas

#filtrando para as vagas de logistica
base_to2 <- base_total %>% filter(str_detect(nome_vaga, "(?i)logistic|(?i)logístic"))  
cont_sites <- base_to2 %>% group_by(site) %>% summarize(n=n()) %>% mutate(perc=n/sum(n))
#conta quantos nomes de vagas possuem as palavas auxiliar, tecnico e especialista
unique(base_to2$nome_vaga)
base_to3 <- base_to2 %>% mutate(nome_vaga=trimws(nome_vaga, which = c("both"), whitespace = "[ \t\r\n]")) %>% #retira espaçoes em branco no inicio e fim
  mutate(nome_vaga=tolower(nome_vaga), nome_vaga=gsub("nova","",nome_vaga))  %>%
  mutate(nome_vaga=iconv(nome_vaga,to="ASCII//TRANSLIT")) %>%
  mutate(titulo=ifelse(str_detect(base_to2$nome_vaga, "(?i)auxiliar|(?i)aux|(?i)assistente|(?i)ajudante")==TRUE,"Assistente",
         ifelse(str_detect(base_to2$nome_vaga, "(?i)operador")==TRUE,"Operador",
         ifelse(str_detect(base_to2$nome_vaga, "(?i)porto|(?i)portuar")==TRUE,"Logística Portuária",
         ifelse(str_detect(base_to2$nome_vaga, "(?i)controlador|(?i)programador|(?i)produção")==TRUE,"Controlador",
                ifelse(str_detect(base_to2$nome_vaga, "(?i)tecnico")==TRUE,"Técnico",
                       ifelse(str_detect(base_to2$nome_vaga, "(?i)especialista|(?i)specialist")==TRUE,"Especialista",
                       ifelse(str_detect(base_to2$nome_vaga, "(?i)analista|(?i)analyst")==TRUE,"Analista",
                      ifelse(str_detect(base_to2$nome_vaga, "(?i)supervisor|(?i)encarregado|(?i)coordenador|(?i)lider|(?i)gerente|(?i)leader|(?i)manager")==TRUE,"Gestão", NA    )))))))))
r <- ocupacoes %>% filter(is.na(titulo)==TRUE)

base_to3[is.na(base_to3$titulo)==TRUE,24] <-"Outros"

ocupacoes_tab <- ocupacoes %>% group_by(titulo) %>% summarize(n=n()) %>% mutate(perc=n/sum(n))
ocupacoes_tab[is.na(ocupacoes_tab$titulo)==TRUE,1]   <- 'Outros'
library(xlsx)
#write.xlsx(as.data.frame(ocupacoes_tab),'tabela_ocupacoes.xlsx', row.names = FALSE)

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
                  gather(a,termo,PC1:PC8) %>% filter(is.na(termo)==FALSE) %>% mutate(termo=iconv(termo,to="ASCII//TRANSLIT")) %>%  #Retirar acentos
                  select(-a) %>% 
                  mutate(termo_ssw=rm_words(termo,sw$stopwords))%>%     #retirar stopwords
                  mutate(termo_ssw=trimws(termo_ssw, which = c("both"), whitespace = "[ \t\r\n]")) %>%   #retirar espaçoes em branco
                  mutate(termo_ssw=tolower(termo_ssw)) #transformando as letras em minusculas

################################################################################
##lemmatiza??o
lemma_dic <- read.delim(file = "lemmatizador.txt", header = FALSE, stringsAsFactors = FALSE, encoding = 'UTF-8')
names(lemma_dic) <- c("stem", "term")
lemma_dic$stem <- iconv(lemma_dic$stem,from = "UTF-8",to="ascii//TRANSLIT")
lemma_dic$term <- iconv(lemma_dic$term,from = "UTF-8",to="ascii//TRANSLIT")
#base de vagas
tidy_base <- base_to3 %>% unnest_tokens(word, conteudo_ssw)
palavras <- tidy_base$word
for (j in 1:length(palavras)){
  comparacao <- palavras[j] == lemma_dic$term
  if (sum(comparacao) == 1){
    palavras[j] <- as.character(lemma_dic$stem[comparacao])
  } else {
    palavras[j] <- palavras[j]
  }
}

tidy_base$words_lemm <- palavras
tidy_vagas <- tidy_base2 %>% group_by(id, conteudo) %>% 
  mutate(conteudo_lem = paste0(words_lemm, collapse = " ")) %>% select("site","id","nome_vaga" ,"conteudo","conteudo_lem" )
tidy_vagas2 <- tidy_vagas[!duplicated(tidy_vagas$id), ]

#lemantizar e reunificar os conteudos por vaga

##base painel
mydata4 <- mydata3 %>% unnest_tokens(word, termo_ssw)
palavras <- mydata4$word

for (j in 1:length(palavras)){
  comparacao <- palavras[j] == lemma_dic$term
  if (sum(comparacao) == 1){
    palavras[j] <- as.character(lemma_dic$stem[comparacao])
  } else {
    palavras[j] <- palavras[j]
  }
}
mydata4$words_lemm <- palavras

mydata5 <- mydata4 %>% 
  group_by(ID, termo) %>% 
  mutate(bars_by_foo = paste0(words_lemm, collapse = " ")) 

#PROXIMO PASSO: CONTAR NA BASE VAGAS AS PALAVRAS DA BASE PAINEL DE ESPECIALISTAS
contagem <- mydata5 %>% select("Palavras_chave","ID","termo","bars_by_foo") %>% arrange(ID) %>% mutate(freq_ws=NA)
for (i in 1:nrow(contagem)){
contagem$freq_ws[i] <- length(grep(contagem$termo_ssw[i], base_to2$conteudo_ssw))
}

############################################################
##Descrição da base de vagas
#Frequencia de palavras
`%notin%` <- Negate(`%in%`)
base_to_tidy <- base_to3 %>% group_by(titulo) %>% unnest_tokens(word, conteudo_ssw) %>%
                count(word, sort = TRUE) %>% filter(word %notin%  c('logistica','logisticanivel','area','trabalho','profissional')) %>% 
                mutate(word = factor(word, levels = rev(unique(word)))) %>%
                group_by(titulo) %>%
                mutate(total=n(), perc=n/total) %>%
                top_n(7) %>%  ungroup %>%  ggplot(aes(reorder(word,perc), perc, fill = titulo)) +
                geom_col(show.legend = FALSE) +
                labs(x = NULL, y = "Frequência de palavras") +
                facet_wrap(~titulo, ncol = 2, scales = "free") +
                coord_flip() +
                theme(axis.text=element_text(size=12),
                      axis.title=element_text(size=18,face="bold"))

base_to_tidy <- base_to3 %>%  unnest_tokens(word, conteudo_ssw) %>%
  count(word, sort = TRUE) %>% filter(word %notin%  c('logistica','area','trabalho','profissional'))%>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  mutate(total=n(), perc=n/total) %>%
  top_n(10) %>%  ungroup %>%  ggplot(aes(reorder(word,perc), perc)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Frequência de palavras")



#Frequência de bigrams
base_bigrams <- base_to3 %>% unnest_tokens(bigram, conteudo_ssw, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE)
#Quais as palavras mais formam bigrams com a palavra dados
base_bigrams2 <- base_bigrams %>% separate(bigram, c("word1", "word2"), sep = " ") %>% 
                 filter(word1 == "dados") %>% ###exemplo, trocar este termo
                 count(word2, sort = TRUE)

#base3 <- base_to2 %>%  filter(str_detect(conteudo, "(?i)dados")) 
#Correlação de palavras
library(widyr)
base_corr <- base_to2 %>% unnest_tokens(word, conteudo_ssw)%>%
  pairwise_cor(word, id, sort = TRUE)
base_corr %>% filter(item1 %in% c("logistica","analise")) %>%
  group_by(item1) %>%
  top_n(6)

#tf-idf usando a base inteira
base_total2 <- base_to3 %>% unnest_tokens(word, conteudo_ssw)   %>%  group_by(titulo) %>% count(titulo ,word, sort=TRUE) %>%        
               bind_tf_idf(word, titulo, n)
#procurando um termo na base
a <- base_to3 %>%
  filter(str_detect(conteudo_ssw, "logisticanivel")) %>%
  select(conteudo)

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

