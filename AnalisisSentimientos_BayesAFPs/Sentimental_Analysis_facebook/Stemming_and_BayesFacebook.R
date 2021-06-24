#20210613 NZepeda, pruebas de stemming con funciones predefinidas


install.packages("corpus")
library("corpus")
library(dplyr)




##Cargamos el archivo de lematización
#Usando la funcion para diccionario en espanol
tab <- read.delim("D:/Github/Datasphere/TextAnalytics/AnalisisSentimientos_BayesAFPs/Sentimental_Analysis_facebook/lemmatization-es.txt", encoding = 'UTF-8', header=FALSE, stringsAsFactors = FALSE)
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



##Cargamos el dataset de comments de AFP Confía
filePath <- "D:/Github/Datasphere/TextAnalytics/AnalisisSentimientos_BayesAFPs/Sentimental_Analysis_facebook/AFPConfiaFacebook.csv"
FBconfia <- read.table(filePath, encoding = 'UTF-8',  header = TRUE, sep = "|")
head(FBconfia)


#Agregamos una columna para almacenar los textos lematizados
FBconfia$text_tokens<-" "

##Verificamos la cantidad de comments
nrow(FBconfia)

for(i in 1:732) {

  FBconfia[i,3]<-sapply(text_tokens(FBconfia[i,1], stemmer = stem_list), paste, collapse=" ")
  }

head(FBconfia, n=11)

##Exportamos el dataset ya Lematizado
write.csv(FBconfia,"D:/Github/Datasphere/TextAnalytics/AnalisisSentimientos_BayesAFPs/Sentimental_Analysis_facebook/STEMMING_AFPConfia.csv", row.names = FALSE)


##---------------------------------------

##Cargamos el dataset de comments de FB para AFP crecer
filePath <- "D:/Github/Datasphere/TextAnalytics/AnalisisSentimientos_BayesAFPs/Sentimental_Analysis_facebook/AFPCrecerFacebook.csv"
FBcrecer <- read.table(filePath, encoding = 'UTF-8',  header = TRUE, sep = "|")


#Agregamos una columna para almacenar los textos lematizados
FBcrecer$text_tokens<-" "

##Verificamos la cantidad de comments
nrow(FBcrecer)




for(i in 1:602) {
  
  FBcrecer[i,3]<-sapply(text_tokens(FBcrecer[i,1], stemmer = stem_list), paste, collapse=" ")
}

head(FBcrecer, n=11)

##Exportamos el dataset ya Lematizado
write.csv(FBcrecer,"D:/Github/Datasphere/TextAnalytics/AnalisisSentimientos_BayesAFPs/Sentimental_Analysis_facebook/STEMMING_AFPcrecer.csv", row.names = FALSE)


###---------------
###---------------
###---------------
##Nos apoyamos de algoritmos de bayes para determinar el sentimiento

#Adicionando paquetes y modulos
library(NLP)
library(ggplot2)
library(tm)
source('D:/Github/Datasphere/TextAnalytics/AnalisisSentimientos_BayesAFPs/Sentimental_Analysis_facebook/classify_polarity.R')
source('D:/Github/Datasphere/TextAnalytics/AnalisisSentimientos_BayesAFPs/Sentimental_Analysis_facebook/create_matrix.R')
source('D:/Github/Datasphere/TextAnalytics/AnalisisSentimientos_BayesAFPs/Sentimental_Analysis_facebook/classify_emotion.R')

corpusCrecer <- read.csv("STEMMING_AFPcrecer.csv")
head(corpusCrecer)
str(corpusCrecer)

#Convertimos a caracter las columna donde se encuentra el texto lematizado
corpusCrecer$V3 <- as.character(corpusCrecer$V3)

# clasificación de la emocion
class_emo = classify_emotion(corpusCrecer$V3, algorithm="bayes", prior=1.0)

emotion = class_emo[,7]


# Seteando no definidos
emotion[is.na(emotion)] = "unknown"

# Seteando polaridad
class_pol = classify_polarity(corpusCrecer$V3, algorithm="bayes", minWordLength = 1)
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
  
  labs(title = "Análisis de Emociones para la mejora del docente\n(Clasificación de comentarios por polaridad)",
       plot.title = element_text(size=12))



##Exportación del DF con el resultado del analisis de sentimientos según bayes
write.csv(sent_df_crecer,"D:/Github/Datasphere/TextAnalytics/AnalisisSentimientos_BayesAFPs/Sentimental_Analysis_facebook/Resulbayes_AFPcrecer.csv", row.names = FALSE)

###-----------
###-----------
###-----------
##Aplicamos bayes al dataset de Confía
###-----------
###-----------

corpusConfia <- read.csv("STEMMING_AFPconfia.csv")
head(corpusConfia)
str(corpusConfia)

#Convertimos a caracter las columna donde se encuentra el texto lematizado
corpusConfia$V3 <- as.character(corpusConfia$V3)

# clasificación de la emocion
class_emo = classify_emotion(corpusConfia$V3, algorithm="bayes", prior=1.0)

emotion = class_emo[,7]


# Seteando no definidos
emotion[is.na(emotion)] = "unknown"

# Seteando polaridad
class_pol = classify_polarity(corpusConfia$V3, algorithm="bayes", minWordLength = 1)
polarity = class_pol[,4]


#PROBANDO C?DIGO DE NAIVE BAYES
sent_df_confia = data.frame(text=corpusConfia, emotion=emotion,polarity=polarity, stringsAsFactors=FALSE)

#Ordenando el Dataframe
sent_df_confia = within(sent_df,emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))


# Grafico de sentimientos
ggplot(sent_df_confia, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="Categorías", y="Comentarios") +
  
  labs(title = "Análisis de Emociones para la mejora del docente\n(Clasificación de comentarios por polaridad)",
       plot.title = element_text(size=12))

##Exportación del DF con el resultado del analisis de sentimientos según bayes
write.csv(sent_df_confia,"D:/Github/Datasphere/TextAnalytics/AnalisisSentimientos_BayesAFPs/Sentimental_Analysis_facebook/Resultbayes_AFPconfia.csv", row.names = FALSE)


