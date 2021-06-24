#20210613 NZepeda, pruebas de stemming con funciones predefinidas
#Ejercicio basado en https://cran.r-project.org/web/packages/corpus/vignettes/stemmer.html

#Revisa ejemplos en http://cartago.lllf.uam.es/grampal/grampal.cgi?m=analiza&csrf=15de3b84b066904cbf046a25e6b8885a&e=clientas

install.packages("corpus")
library("corpus")
library(dplyr)
text <- "love loving lovingly loved lover lovely love"
text_tokens(text, stemmer = "en") # english stemmer

#Espanol
text <- "Cliente Clientes Clientas Clientecito"
text_tokens(text, stemmer = "es") # espa stemmer


text <- "Reclamar Reclame Reclamo Reclamasteis"
text_tokens(text, stemmer = "es") # espa stemmer


stem_hunspell <- function(term) {
  # look up the term in the dictionary
  stems <- hunspell::hunspell_stem(term)[[1]]
  
  if (length(stems) == 0) { # if there are no stems, use the original term
    stem <- term
  } else { # if there are multiple stems, use the last one
    stem <- stems[[length(stems)]]
  }
  
  stem
}

text <- "Reclamar Reclame Reclamo Reclamasteis"
text_tokens(text, stemmer = stem_hunspell)

text <- "love loving lovingly loved lover lovely love"
text_tokens(text, stemmer = stem_hunspell)


getwd()
setwd("/Users/nelsonzepeda/Desktop/Datasphere/TextAnalytics-main/Stemming/lemmatization-lists-master")
# download the list

#https://github.com/michmech/lemmatization-lists

# extract the contents
tab <- read.delim("lemmatization-en.txt", header=FALSE, stringsAsFactors = FALSE)
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

text <- "love loving lovingly loved lover lovely love"
text_tokens(text, stemmer = stem_list)


#Usando la funcion para diccionario en espanol
tab <- read.delim("lemmatization-es.txt", header=FALSE, stringsAsFactors = FALSE)
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


text <- "Reclamar Reclame Reclamo Reclamasteis"
text_tokens(text, stemmer = stem_list)


filePath <- "/Users/nelsonzepeda/Desktop/Datasphere/TextAnalytics-main/Stemming/TelcosSV/CLARO_202006.csv"

text <- readLines(filePath)

text

text <- "queremos señal de internet estable"
ejemplo<-text_tokens(text, stemmer = stem_list)
ejemplo
str(ejemplo)

result<-sapply(ejemplo, paste, collapse=" ")
result



filePath <- "D:/Github/Datasphere/TextAnalytics/AnalisisSentimientos_BayesAFPs/Sentimental_Analysis_facebook/AFPConfiaFacebook.csv"
text <- read.table(filePath, header = TRUE, sep = "|")
str(text)

head(text)
which(is.na(text))


sapply(text_tokens(text[400,2], stemmer = stem_list), paste, collapse=" ")

text$text_tokens<-" "
head(text)

nrow(text)


for(i in 1:100) {

  text[i,3]<-sapply(text_tokens(text[i,2], stemmer = stem_list), paste, collapse=" ")
  }

head(text, n=11)

write.csv(text,"/Users/nelsonzepeda/Desktop/Datasphere/TextAnalytics-main/Stemming/TelcosSV/STEMMING_CLARO_202006.csv", row.names = FALSE)


