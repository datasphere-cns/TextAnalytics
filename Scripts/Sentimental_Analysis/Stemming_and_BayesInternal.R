#20210613 NZepeda, pruebas de stemming con funciones predefinidas


install.packages("corpus")
library("corpus")
library(dplyr)


oldw <- getOption("warn")

options(warn = -1)

##Cargamos el archivo de lematización
#Usando la funcion para diccionario en espanol
tab <- read.delim("C:/AFP-TextAnalysis/scripts/Sentimental_Analysis/lemmatization-es.txt", encoding = 'UTF-8', header=FALSE, stringsAsFactors = FALSE)
names(tab) <- c("stem", "term")
head(tab)


stem_list <- function(term) {
  i <- match(term, tab$term)
  if (is.na(i)) {
    stem <- term
  } else {
    stem <- tab$stem[[i]]
  }
  stem
}



##Cargamos el dataset de comments de AFP Crecer

library(tidyverse)
DATASET_AFPCrecer<- read_csv( "C:/Users/nelso/Downloads/AWS/AWS/NLP/mensajes_tickets_curso/mensajes_tickets_curso.csv", 
                              col_names = TRUE, col_types = cols(
                                Fuente = col_character(),
                                Id = col_character(),
                                Fecha  = col_character(),
                                Canal    = col_character(),
                                Origen    = col_character(),
                                Mensaje   = col_character() 
                              )
)


#Agregamos una columna para almacenar los textos lematizados
DATASET_AFPCrecer$text_tokens<-" "

##Verificamos la cantidad de comments
nrow(DATASET_AFPCrecer)
head(DATASET_AFPCrecer)
DATASET_AFPCrecer <- DATASET_AFPCrecer %>% rename( text= Mensaje)


for(i in 1:515691) {

  DATASET_AFPCrecer[i,8]<-sapply(text_tokens(DATASET_AFPCrecer[i,7], stemmer = stem_list), paste, collapse=" ")
  }



head(DATASET_AFPCrecer, n=11)

##Exportamos el dataset ya Lematizado
write.csv(DATASET_AFPCrecer,"C:/Users/nelso/Downloads/AWS/AWS/NLP/mensajes_tickets_curso/STEMMING_AFPCrecer_Interna.csv", row.names = FALSE)



###---------------
###---------------
###---------------
##Nos apoyamos de algoritmos de bayes para determinar el sentimiento

#Adicionando paquetes y modulos
library(NLP)
library(ggplot2)
library(tm)
source('C:/AFP-TextAnalysis/scripts/Sentimental_Analysis/classify_polarity.R')
source('C:/AFP-TextAnalysis/scripts/Sentimental_Analysis/create_matrix.R')
source('C:/AFP-TextAnalysis/scripts/Sentimental_Analysis/classify_emotion.R')

corpusCrecer <- read.csv("C:/Users/nelso/Downloads/AWS/AWS/NLP/mensajes_tickets_curso/STEMMING_AFPCrecer_Interna.csv")
head(corpusCrecer)
str(corpusCrecer)

#Convertimos a caracter las columna donde se encuentra el texto lematizado
corpusCrecer$text_tokens <- as.character(corpusCrecer$text_tokens)
head(corpusCrecer)

corpusCrecer<-corpusCrecer[complete.cases(corpusCrecer[,]),]

corpusCrecer<-corpusCrecer[sample(nrow(corpusCrecer), 2000), ]

# clasificación de la emocion

classify_polarity("no me gusta", algorithm="bayes", minWordLength = 1)

classify_polarity("mal servicio", algorithm="bayes", minWordLength = 1)
classify_polarity("excelente servicio", algorithm="bayes", minWordLength = 1)
classify_polarity("no me llego el correo, cuando me responderan?", algorithm="bayes", minWordLength = 1)

classify_polarity("pesimo mal servicio", algorithm="bayes", minWordLength = 1)
classify_polarity("ya me voy", algorithm="bayes", minWordLength = 1)
classify_polarity("amo a la AFP", algorithm="bayes", minWordLength = 1)
classify_polarity("amo a la AFP y sus empleados", algorithm="bayes", minWordLength = 1)
classify_polarity("no me sirve", algorithm="bayes", minWordLength = 1)
classify_polarity("servicio detestable", algorithm="bayes", minWordLength = 1)
classify_polarity("correo", algorithm="bayes", minWordLength = 1)
classify_polarity("nelson", algorithm="bayes", minWordLength = 1)
classify_polarity("agradec", algorithm="bayes", minWordLength = 1)
classify_polarity("agresion", algorithm="bayes", minWordLength = 1)
classify_polarity("listar", algorithm="bayes", minWordLength = 1)
classify_polarity("perdón me equivocar . . solo pedir uno 23 %", algorithm="bayes", minWordLength = 1)
classify_polarity("agresion agradec", algorithm="bayes", minWordLength = 1)



class_emo = classify_emotion(corpusCrecer$text_tokens, algorithm="bayes", prior=1.0)

emotion = class_emo[,7]


# Seteando no definidos
emotion[is.na(emotion)] = "unknown"

# Seteando polaridad
corpusCrecer[250,]
corpusCrecer[250,8]
classify_polarity(corpusCrecer[250,8], algorithm="bayes", minWordLength = 1)

class_pol = classify_polarity(corpusCrecer$text_tokens, algorithm="bayes", minWordLength = 1)


polarity = class_pol[,4]



#PROBANDO C?DIGO DE NAIVE BAYES
sent_df_crecer = data.frame(text=corpusCrecer, emotion=emotion,polarity=polarity, stringsAsFactors=FALSE)


#Ordenando el Dataframe
sent_df_crecer = within(sent_df_crecer,emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))


# Grafico de sentimientos
ggplot(sent_df_crecer, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="Categorías", y="Comentarios") +
  
  labs(title = "Análisis de Emociones - Facebook - AFP Crecer\n(Clasificación de comentarios por polaridad)",
       plot.title = element_text(size=12))



##Exportación del DF con el resultado del analisis de sentimientos según bayes
write.csv(sent_df_crecer,"C:/Users/nelso/Downloads/AWS/AWS/NLP/mensajes_tickets_curso/Resulbayes_AFPcrecer.csv", row.names = FALSE)
