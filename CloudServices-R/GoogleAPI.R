install.packages("twitteR")
library(twitteR)
install.packages("devtools")
library(devtools)

install.packages("https://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz",
                 repos=NULL, method="libcurl")
install.packages("https://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz",
                 repos=NULL, method="libcurl")


library("Rstem")
library("sentiment")

install.packages("plotly")
library("plotly")
library(dplyr)
library(wordcloud)


classify_emotion("poor", algorithm='bayes')

classify_emotion("amor", algorithm='bayes', language = "spanish")
classify_polarity("malo", algorithm='bayes', language = "spanish")


install.packages("translateR")
library(translateR)
data(enron)
str(enron)
getGoogleLanguages()


textdf<-data.frame('devuelvan lo robado')
str(textdf)
colnames(textdf) <- c("text")
textdf$text <- as.character(textdf$text)  
textdf

translated<-translate(dataset = textdf,
          content.field='text',
          google.api.key = 'AIzaSyBc4GH-TkYNvSL7DdLzIziJO4RYnMF8xqI',
          source.lang = 'es',
          target.lang = 'en')

str(translated)

classify_emotion(translated, algorithm='bayes')
classify_polarity(translated, algorithm='bayes')




textdf<-data.frame('pesimo servicio al cliente')
str(textdf)
colnames(textdf) <- c("text")
textdf$text <- as.character(textdf$text)  
textdf

translated<-translate(dataset = textdf,
                      content.field='text',
                      google.api.key = 'AIzaSyBc4GH-TkYNvSL7DdLzIziJO4RYnMF8xqI',
                      source.lang = 'es',
                      target.lang = 'en')

str(translated)

classify_emotion(translated, algorithm='bayes')
classify_polarity(translated, algorithm='bayes')




textdf<-data.frame('excelente servicio al cliente')
str(textdf)
colnames(textdf) <- c("text")
textdf$text <- as.character(textdf$text)  
textdf

translated<-translate(dataset = textdf,
                      content.field='text',
                      google.api.key = 'AIzaSyBc4GH-TkYNvSL7DdLzIziJO4RYnMF8xqI',
                      source.lang = 'es',
                      target.lang = 'en')

str(translated)

classify_emotion(translated, algorithm='bayes')
classify_polarity(translated, algorithm='bayes')


