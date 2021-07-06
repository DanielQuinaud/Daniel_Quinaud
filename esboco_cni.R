library(readODS)
library(stringr)
library(data.table)
library(tidytext)
library(tidyverse)
library(readxl)
library(quanteda)
rm(list=ls())
#Lendo as Bases de dados do web scrapping t0
setwd('C:/Users/daniel.silva/Documents/Daniel/Text Mining')
#x1 <- read.csv("Indeed_t0.csv", sep=',', stringsAsFactors = FALSE)
#x2 <- read.csv("Infojobs_t0.csv", sep=",", stringsAsFactors = FALSE)
#x3 <- read.csv("SiteVagas_t0.csv", sep=",", stringsAsFactors = FALSE)
#x4 <- read.csv("vagasCertas2021_05_10_Yuan_t0_wide.csv", sep=",")
t0 <- fread("base_t0.csv", sep=',', stringsAsFactors = FALSE, encoding = 'UTF-8', header=TRUE)
info <- fread("Infojobs_re_scraping.csv", sep=',', stringsAsFactors = FALSE, encoding = 'UTF-8', header=TRUE)

info$site <- 'infojobs'
info$beneficios <- '-'
info$data <- '-'
info$request <- '-'
info$dif_data <- '-'

info <- info[,c("V1","id","site","adicional","beneficios","codigo","conteudo",'data',"empresa" ,"jornada",         
                "localidade", "profissao","regime","salario","url" ,"vagas", "data publicada","data do scraping",'request',
                'dif_data')]

colnames(info)[10] <- 'horario'
t0$`data do scraping` <- as.character(t0$`data do scraping`)
t0 <- rbind(t0 %>% filter(site!='infojobs'), info)
#lista de stopwords em portugues
sw <- read.delim('stopwords.txt', header = TRUE, encoding='UTF-8')   #lendo a lista de stopwords
sw$stopwords <- as.character(sw$stopwords)
sw$stopwords <- trimws(sw$stopwords, which = c("both"), whitespace = "[ \t\r\n]") #retirando espações em branco no inicio e final 
#função que retira stopwords de um texto
rm_words <- function(string, words) {
  stopifnot(is.character(string), is.character(words))
  spltted <- strsplit(string, " ", fixed = TRUE) # fixed = TRUE for speedup
  vapply(spltted, function(x) paste(x[!tolower(x) %in% words], collapse = " "), character(1))
}
#em ingles
sw_en <- as.data.frame(stopwords("english"))
colnames(sw_en) <- 'sw_ingles'


#base consolidada
#colnames(x1)[11] <- 'conteudo'
#x1$beneficios <- "-"
#x2$beneficios <- "-"
#x1$jornada <- "-"
#x3$jornada <- "-"
#x3$regime <- "-"
#x2$X3_situacao.da.vaga <- "-"
#x2$X3_data.do.request <- "-"

#x3 <- x3 %>% select(-beneficios)
#coluna beneficios foi removida de x3
#x2 <- x2[,colnames(x1)]
#x3 <- x3[,colnames(x1)]

#base_to <- rbind(x1,x2,x3)
#base_to$site <- qdapRegex::ex_between(base_to$url, "https://", ".com")
#base_to <- base_to[,c(1,22,2:21)]
#base_to$site <- gsub("www.","",base_to$site,)
#base_to$site <- gsub("br.","",base_to$site,)

base_total <- t0 %>% rename(nome_vaga=profissao) %>%
  mutate(nome_vaga=trimws(nome_vaga, which = c("both"), whitespace = "[ \t\r\n]")) %>%
  mutate(nome_vaga=iconv(nome_vaga,from='UTF-8',to="ASCII//TRANSLIT")) %>%
  mutate(nome_vaga=tolower(nome_vaga)) %>%                   
  #abaixo, arrumando a coluna conteudo
  mutate(conteudo=iconv(conteudo,from='UTF-8',to="ASCII//TRANSLIT")) %>%   #retira acentos
  mutate(conteudo=str_replace_all(conteudo,"[/''?.,;:()]", "")) %>% #retira pontuações e outros simbolos
  mutate(conteudo=gsub("Area e especializacao profissional","",conteudo),
         conteudo=gsub("Nivel hierarquico","",conteudo),
         conteudo=gsub("Local de trabalho","",conteudo)) %>%
  mutate(conteudo_ssw=rm_words(conteudo,sw$stopwords)) %>%  #retirar stopwords
  mutate(conteudo_ssw=rm_words(conteudo_ssw,sw_en$sw_ingles))%>% #retirar stopwords em ingles
  mutate(conteudo_ssw=trimws(conteudo_ssw, which = c("both"), whitespace = "[ \t\r\n]")) %>% #retira espaçoes em branco no inicio e fim
  mutate(conteudo_ssw=tolower(conteudo_ssw))     #transformando todas as letras em minusculas
#############padronizando as outras variaveis
#write.csv2(base_total, 'base_to_tratada.csv', row.names = FALSE)
#base_total <- fread('base_to_tratada.csv')
base_total2 <- base_total %>% mutate(adicional_ssw=iconv(adicional,from='UTF-8',to="ASCII//TRANSLIT")) %>%   #retira acentos
  mutate(adicional_ssw=str_replace_all(adicional_ssw,"[/''?.,;:()]", "")) %>% #retira pontuações e outros simbolos
  mutate(adicional_ssw=rm_words(adicional_ssw,sw$stopwords)) %>%  #retirar stopwords
  mutate(adicional_ssw=rm_words(adicional_ssw,sw_en$sw_ingles))%>% #retirar stopwords em ingles
  mutate(adicional_ssw=trimws(adicional_ssw, which = c("both"), whitespace = "[ \t\r\n]")) %>% #retira espaçoes em branco no inicio e fim
  mutate(adicional_ssw=tolower(adicional_ssw)) %>%
  mutate(beneficios_ssw=iconv(beneficios,from='UTF-8',to="ASCII//TRANSLIT")) %>%   #retira acentos
  mutate(beneficios_ssw=str_replace_all(beneficios_ssw,"[/''?.,;:()]", "")) %>% #retira pontuações e outros simbolos
  mutate(beneficios_ssw=rm_words(beneficios_ssw,sw$stopwords)) %>%  #retirar stopwords
  mutate(beneficios_ssw=rm_words(beneficios_ssw,sw_en$sw_ingles))%>% #retirar stopwords em ingles
  mutate(beneficios_ssw=trimws(beneficios_ssw, which = c("both"), whitespace = "[ \t\r\n]")) %>% #retira espaçoes em branco no inicio e fim
  mutate(beneficios_ssw=tolower(beneficios_ssw)) %>%
  mutate(conteudo_ssw=gsub("\n","",conteudo_ssw),
         conteudo_ssw=gsub("\r","",conteudo_ssw))

a <- base_total2 %>%
  filter(str_detect(conteudo_ssw, "\n")) %>%
  select(conteudo_ssw)
#write.csv2(base_total2, 'base_to_tratada4.csv', row.names = FALSE)
#####################################################################################################
####RECOMEÇAR AQUI
base_total2 <- fread('base_to_tratada4.csv')
#Juntando as colunas de informações
base_total2s <- base_total2 %>% mutate(conteudo_total=paste0(conteudo_ssw," ",adicional_ssw," ",beneficios_ssw))




#filtrando para as vagas de logistica
base_to2 <- base_total2s %>% filter(str_detect(nome_vaga, "(?i)logistic"))  
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
r <- base_to3 %>% filter(is.na(titulo)==TRUE)

base_to3[is.na(base_to3$titulo)==TRUE,22] <-"Outros"

ocupacoes_tab <- base_to3 %>% group_by(titulo) %>% summarize(n=n()) %>% mutate(perc=n/sum(n))
#ocupacoes_tab[is.na(ocupacoes_tab$titulo)==TRUE,1]   <- 'Outros'
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
  gather(a,termo,PC1:PC8) %>% filter(is.na(termo)==FALSE) %>% mutate(termo=iconv(termo,from='UTF-8', to="ASCII//TRANSLIT")) %>%  #Retirar acentos
  select(-a) %>% 
  mutate(termo_ssw=rm_words(termo,sw$stopwords))%>%     #retirar stopwords
  mutate(termo_ssw=trimws(termo_ssw, which = c("both"), whitespace = "[ \t\r\n]")) %>%   #retirar espaçoes em branco
  mutate(termo_ssw=tolower(termo_ssw)) #transformando as letras em minusculas

################################################################################
##lemmatização
lemma_dic <- read.delim(file = "lemmatizador.txt", header = FALSE, stringsAsFactors = FALSE, encoding = 'UTF-8')
names(lemma_dic) <- c("stem", "term")
lemma_dic$stem <- iconv(lemma_dic$stem,from = "UTF-8",to="ascii//TRANSLIT")
lemma_dic$term <- iconv(lemma_dic$term,from = "UTF-8",to="ascii//TRANSLIT")
#base de vagas
base_to3$V1 <- row.names(base_to3)
tidy_base <- base_to3 %>% unnest_tokens(word, conteudo_total)


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
tidy_vagas <- tidy_base %>% group_by(id, conteudo) %>% 
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
  group_by(ID, termo,Ocupação, Item) %>% 
  mutate(bars_by_foo = paste0(words_lemm, collapse = " ")) 

 write.csv2(contagem,'painel_preparado.csv', row.names = FALSE)
 write.csv2(tidy_vagas2,'tidy_vagas2.csv', row.names = FALSE)
 

############################################################3
#RECOMEÇAR AQUI
#PROXIMO PASSO: CONTAR NA BASE VAGAS AS PALAVRAS DA BASE PAINEL DE ESPECIALISTAS
contagem <- mydata5 %>% select("Palavras_chave","ID","termo","bars_by_foo") %>% arrange(ID) %>% mutate(freq_ws=NA)%>%
            mutate(bars_by_foo=ifelse(termo==' IA', ' ia ',bars_by_foo))   

contagem <- fread('painel_preparado.csv')
tidy_vagas2 <- fread('tidy_vagas2.csv')

for (i in 1:nrow(contagem)){
  contagem$freq_ws[i] <- length(grep(contagem$bars_by_foo[i], tidy_vagas2$conteudo_lem))
}
contagem2 <- contagem %>% mutate(perc=freq_ws/nrow(tidy_vagas2))
write.xlsx(as.data.frame(contagem2),'compar_vagas.xlsx', row.names = FALSE)

##########################################################################################
##Nivel de formação nas vagas

tidy_vagas3 <- tidy_vagas2 %>%  
  mutate(titulo=ifelse(str_detect(tidy_vagas2$nome_vaga, "(?i)auxiliar|(?i)aux|(?i)assistente|(?i)ajudante")==TRUE,"Assistente",
         ifelse(str_detect(tidy_vagas2$nome_vaga, "(?i)operador")==TRUE,"Operador",
         ifelse(str_detect(tidy_vagas2$nome_vaga, "(?i)porto|(?i)portuar")==TRUE,"Outros",
         ifelse(str_detect(tidy_vagas2$nome_vaga, "(?i)tecnico")==TRUE,"Técnico",
         ifelse(str_detect(tidy_vagas2$nome_vaga, "(?i)especialista|(?i)specialist|(?i)analista|(?i)analyst|(?i)controlador|(?i)programador|(?i)produção")==TRUE,"Especialista",
         ifelse(str_detect(tidy_vagas2$nome_vaga, "(?i)supervisor|(?i)encarregado|(?i)coordenador|(?i)lider|(?i)gerente|(?i)leader|(?i)manager")==TRUE,"Gestão", NA))))))) %>%
  mutate(conteudo_ssw=iconv(conteudo,from='UTF-8',to="ASCII//TRANSLIT")) %>%   #retira acentos
  mutate(conteudo_ssw=str_replace_all(conteudo_ssw,"[/''?.,;:()]", "")) %>% #retira pontuações e outros simbolos
  mutate(conteudo_ssw=rm_words(conteudo_ssw,sw$stopwords)) %>%  #retirar stopwords
  mutate(conteudo_ssw=rm_words(conteudo_ssw,sw_en$sw_ingles))%>% #retirar stopwords em ingles
  mutate(conteudo_ssw=trimws(conteudo_ssw, which = c("both"), whitespace = "[ \t\r\n]")) %>% #retira espaçoes em branco no inicio e fim
  mutate(conteudo_ssw=tolower(conteudo_ssw)) %>%
mutate(qualificacao1=ifelse(str_detect(conteudo_ssw, "(?i)ensino medio|(?i)nivel medio|(?i)curso medio")==TRUE,"Nível Médio",NA),
       qualificacao2=ifelse(str_detect(conteudo_ssw, "(?i)ensino fundamental|(?i)nivel fundamental|(?i)curso fundamental|(?i)fundamental completo")==TRUE,"Nível Fundamental",NA),
       qualificacao3=ifelse(str_detect(conteudo_ssw, "(?i)ensino superior|(?i)nivel superior|(?i)curso superior|(?i)superior completo|(?i)graduacao|(?i)formacao superior|(?i)diploma")==TRUE,"Nível Superior", NA),
       qualificacao4=ifelse(str_detect(conteudo_ssw, "(?i)tecnico logistica|(?i)tecnico logistico|(?i)ensino tecnico|(?i)técnico em|(?i)curso tecnico")==TRUE,"Nível Técnico", NA))  %>%
    mutate(sem_info=ifelse(is.na(qualificacao1)==FALSE |is.na(qualificacao2)==FALSE |
                             is.na(qualificacao3)==FALSE |is.na(qualificacao4)==FALSE,1,0))
           
                    
  cont1 <- tidy_vagas3 %>% group_by(sem_info) %>% summarize(n=n())

  
table(tidy_vagas3$qualificacao1)
table(tidy_vagas3$qualificacao2)
table(tidy_vagas3$qualificacao3)
table(tidy_vagas3$qualificacao4)
a <- tidy_vagas2 %>%
  filter(str_detect(conteudo_ssw, " tecnico ")) %>%
  select(conteudo_ssw)



tidy_vagas3[is.na(tidy_vagas3$qualificacao1),8] <- "nao"
tidy_vagas3[is.na(tidy_vagas3$qualificacao2),9] <- "nao"
tidy_vagas3[is.na(tidy_vagas3$qualificacao3),10] <- "nao"
tidy_vagas3[is.na(tidy_vagas3$qualificacao4),11] <- "nao"

group_tidy1 <- tidy_vagas3 %>% group_by(titulo, qualificacao1) %>% summarize(n=n())
group_tidy2 <- tidy_vagas3 %>% group_by(titulo, qualificacao2) %>% summarize(n=n())
group_tidy3 <- tidy_vagas3 %>% group_by(titulo, qualificacao3) %>% summarize(n=n())
group_tidy4 <- tidy_vagas3 %>% group_by(titulo, qualificacao4) %>% summarize(n=n())

group_tidy1 <- as.data.frame(group_tidy1)
group_tidy2 <- as.data.frame(group_tidy2)
group_tidy3 <- as.data.frame(group_tidy3)
group_tidy4 <- as.data.frame(group_tidy4)

group_tidy <- cbind(group_tidy1,group_tidy3,group_tidy4)
library(xlsx)
write.xlsx(group_tidy2, 'qualificacao_2.xlsx', row.names = FALSE)


b <- tidy_vagas3 %>%  filter(sem_info==1) %>% group_by(titulo) %>% summarize(n=n())

sum(b$n)


############################################################
##Descrição da base de vagas - Relatório 3
#Frequencia de palavras
`%notin%` <- Negate(`%in%`)
base_to_tidy <- base_to3 %>% group_by(titulo) %>% unnest_tokens(word, conteudo_ssw) %>%
  count(word, sort = TRUE) %>% filter(word %notin%  c('logistica','logisticanivel','area','trabalho','profissional')) %>% 
  group_by(titulo) %>%
  mutate(total=n(), perc=n/total) %>% filter(titulo!='Logística Portuária') %>%
  arrange(desc(perc), .by_group = TRUE) %>% ungroup() %>%
  mutate_at(vars('titulo','word'), ~as.factor(.)) %>% ungroup() %>%
  arrange(titulo) %>% group_by(titulo) %>%
  arrange(titulo, -perc, .by_group = TRUE) %>% mutate(rank = row_number()) %>% 
  top_n(8, perc) %>% ungroup() 
#Gráfico
base_to_tidy %>% ggplot() +geom_bar(aes(desc(rank), perc, fill = titulo), stat = 'identity', show.legend = FALSE)+
   geom_text(aes(y=0, label=word,x = desc(rank)),color="black",position=position_dodge(width = 0.9), 
             hjust=1.2, show.legend = FALSE, size=4,fontface="bold") +
  labs(x = NULL, y = "Frequencia Relativa") +
  facet_wrap(~titulo, ncol = 2, scales = "free") +
  coord_flip(clip = "off", expand = TRUE) +
  #ylim(-200,1400) +
  #guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_text(size=10,face="bold",color = "black",),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        strip.text.x = element_text(size = 16, color = "black", face = "bold"),legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm")) 


base_to_tidy <- base_to3 %>%  unnest_tokens(word, conteudo_ssw) %>%
  count(word, sort = TRUE) %>% filter(word %notin%  c('logistica','area','trabalho','profissional'))%>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  mutate(total=n(), perc=n/total) %>%
  top_n(10) %>%  ungroup %>%  ggplot(aes(reorder(word,perc), perc)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Frequência de palavras")



#Frequência de bigrams
bigram_tidy <- base_to3 %>% group_by(titulo) %>% unnest_tokens(bigram, conteudo_ssw, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE) %>% #filter(word %notin%  c('logistica','logisticanivel','area','trabalho','profissional')) %>% 
  group_by(titulo) %>%
  mutate(total=n(), perc=n/total) %>% filter(titulo!='Logística Portuária') %>%
  arrange(desc(perc), .by_group = TRUE) %>% ungroup() %>%
  mutate_at(vars('titulo','bigram'), ~as.factor(.)) %>% ungroup() %>%
  arrange(titulo) %>% group_by(titulo) %>%
  arrange(titulo, -perc, .by_group = TRUE) %>% mutate(rank = row_number()) 

#Gráfico bigrams
bigram_tidy %>% filter(titulo %notin% c('Técnico','Outros')) %>% slice_max(perc, n = 5) %>% ungroup()%>%
  ggplot()+geom_col(aes(desc(rank), perc, fill = titulo), stat = 'identity', show.legend = FALSE)+
  geom_text(aes(y=0, label=bigram,x = desc(rank)),color="black",position=position_dodge(width = 0.9), 
            hjust=1.0, show.legend = FALSE, size=5,fontface="bold") +
  labs(x = NULL, y = "Frequencia Relativa") +
  facet_wrap(~titulo, ncol = 2, scales = "free") +
  coord_flip(clip = "off", expand = TRUE) +
  #ylim(-200,1400) +
  #guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_text(size=10,face="bold",color = "black",),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        strip.text.x = element_text(size = 16, color = "black", face = "bold"),legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm")) 

ggsave("bigram.pdf", width = 4, height = 4)





#Quais as palavras mais formam bigrams com a palavra dados
bigram_tidy2 <- bigram_tidy %>% separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(word1 == "analisar") %>% ###exemplo, trocar este termo
  count(word2, sort = TRUE)

#base3 <- base_to2 %>%  filter(str_detect(conteudo, "(?i)dados")) 
#Correlação de palavras
library(widyr)
memory.size(max=10^20)
gc(TRUE)
base_corr <- base_to3 %>% unnest_tokens(word, conteudo_ssw)%>% filter(site=='catho')%>%
  pairwise_cor(word, id, sort = TRUE)
base_corr %>% filter(item1 %in% c("logistica","analise")) %>%
  group_by(item1) %>%
  top_n(6)

#tf-idf 
base_total2 <- base_to3 %>% unnest_tokens(word, conteudo_ssw) %>% filter(word %notin% c('prsera','bra','latam','cm'))%>%
  group_by(titulo) %>% count(titulo ,word, sort=TRUE) %>%        
  bind_tf_idf(word, titulo, n) %>% arrange(tf_idf,.by_group = TRUE) %>% ungroup() %>%
  ungroup() %>%
  arrange(titulo) %>% group_by(titulo) %>%
  arrange(titulo, -tf_idf, .by_group = TRUE) %>% mutate(rank = row_number()) 


base_total2 %>% filter(titulo %in% c('Analista','Assistente','Gestão','Operador')) %>% 
  slice_max(tf_idf, n =6) %>% ungroup()%>%
  ggplot()+geom_col(aes(desc(rank), tf_idf, fill = titulo), stat = 'identity', show.legend = FALSE)+
  geom_text(aes(y=0, label=word,x = desc(rank)),color="black",position=position_dodge(width = 0.9), 
            hjust=1.0, show.legend = FALSE, size=6,fontface="bold") +
  labs(x = NULL, y = "Frequencia Relativa") +
  facet_wrap(~titulo, ncol = 2, scales = "free") +
  coord_flip(clip = "off", expand = TRUE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_text(size=14,face="bold",color = "black",),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        strip.text.x = element_text(size = 20, color = "black", face = "bold"),legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm")) 




#procurando um termo na base
a <- base_total %>%
  filter(str_detect(conteudo_ssw, "glp")) %>%
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

