#20210613 NZepeda, pruebas de stemming con funciones predefinidas


install.packages("corpus")
library("corpus")
library(dplyr)




##Cargamos el archivo de lematización
#Usando la funcion para diccionario en espanol
tab <- read.delim("D:/Github/Datasphere/TextAnalytics/AnalisisSentimientos_BayesAFPs/Sentimental_Analysis_Instagram/lemmatization-es.txt", encoding = 'UTF-8', header=FALSE, stringsAsFactors = FALSE)
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
filePath <- "D:/Github/Datasphere/TextAnalytics/AnalisisSentimientos_BayesAFPs/Sentimental_Analysis_Instagram/AFPConfiaInstagram.csv"
instaconfia <- read.table(filePath, encoding = 'UTF-8',  header = TRUE, sep = "|")
head(instaconfia)


#Agregamos una columna para almacenar los textos lematizados
instaconfia$text_tokens<-" "

##Verificamos la cantidad de comments
nrow(instaconfia)

for(i in 1:213) {

  instaconfia[i,3]<-sapply(text_tokens(instaconfia[i,1], stemmer = stem_list), paste, collapse=" ")
  }

head(instaconfia, n=11)

##Exportamos el dataset ya Lematizado
write.csv(instaconfia,"D:/Github/Datasphere/TextAnalytics/AnalisisSentimientos_BayesAFPs/Sentimental_Analysis_Instagram/STEMMINGInsta_AFPConfia.csv", row.names = FALSE)


##---------------------------------------

##Cargamos el dataset de comments de FB para AFP crecer
filePath <- "D:/Github/Datasphere/TextAnalytics/AnalisisSentimientos_BayesAFPs/Sentimental_Analysis_Instagram/AFPCrecerInstagram.csv"
instacrecer <- read.table(filePath, encoding = 'UTF-8',  header = TRUE, sep = "|")


#Agregamos una columna para almacenar los textos lematizados
instacrecer$text_tokens<-" "

##Verificamos la cantidad de comments
nrow(instacrecer)




for(i in 1:66) {
  
  instacrecer[i,3]<-sapply(text_tokens(instacrecer[i,1], stemmer = stem_list), paste, collapse=" ")
}

head(instacrecer, n=11)

##Exportamos el dataset ya Lematizado
write.csv(instacrecer,"D:/Github/Datasphere/TextAnalytics/AnalisisSentimientos_BayesAFPs/Sentimental_Analysis_Instagram/STEMMINGInsta_AFPcrecer.csv", row.names = FALSE)


###---------------
###---------------
###---------------
##Nos apoyamos de algoritmos de bayes para determinar el sentimiento

#Adicionando paquetes y modulos
library(NLP)
library(ggplot2)
library(tm)
source('D:/Github/Datasphere/TextAnalytics/AnalisisSentimientos_BayesAFPs/Sentimental_Analysis_Instagram/classify_polarity.R')
source('D:/Github/Datasphere/TextAnalytics/AnalisisSentimientos_BayesAFPs/Sentimental_Analysis_Instagram/create_matrix.R')
source('D:/Github/Datasphere/TextAnalytics/AnalisisSentimientos_BayesAFPs/Sentimental_Analysis_Instagram/classify_emotion.R')

corpusCrecer <- read.csv("STEMMINGInsta_AFPcrecer.csv")
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
  
  labs(title = "Análisis de Emociones - Instagram - Crecer \n(Clasificación de comentarios por polaridad)",
       plot.title = element_text(size=12))



##Exportación del DF con el resultado del analisis de sentimientos según bayes
write.csv(sent_df_crecer,"D:/Github/Datasphere/TextAnalytics/AnalisisSentimientos_BayesAFPs/Sentimental_Analysis_Instagram/ResulbayesInsta_AFPcrecer.csv", row.names = FALSE)

###-----------
###-----------
###-----------
##Aplicamos bayes al dataset de Confía
###-----------
###-----------

corpusConfia <- read.csv("STEMMINGInsta_AFPconfia.csv")
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
sent_df_confia = within(sent_df_confia,emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))


# Grafico de sentimientos
ggplot(sent_df_confia, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="Categorías", y="Comentarios") +
  
  labs(title = "Análisis de Emociones - Instagram - Confia \n(Clasificación de comentarios por polaridad)",
       plot.title = element_text(size=12))

##Exportación del DF con el resultado del analisis de sentimientos según bayes
write.csv(sent_df_confia,"D:/Github/Datasphere/TextAnalytics/AnalisisSentimientos_BayesAFPs/Sentimental_Analysis_Instagram/ResultbayesInsta_AFPconfia.csv", row.names = FALSE)


