install.packages("NLP")
library("NLP")
library("tm")
#library("SnowballC")
library("RColorBrewer")
library("wordcloud")

filePath <- "/Users/nelsonzepeda/Desktop/Datasphere/TextAnalytics-main/Stemming/TelcosSV/CLARO_STEM_202006_1500.csv"

text <- readLines(filePath)
docs <- Corpus(VectorSource(text))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, content_transformer(tolower)) #pasamos todo a minusculas
docs <- tm_map(docs, removeNumbers) #quitamos numeros
docs <- tm_map(docs, removeWords, stopwords("spanish")) #fuera palabras comunes
docs <- tm_map(docs, removeWords, c("que", "por","para","por","los","las","el","la","lo","el","senor","senora","senores","tener","senoras","presidente","presidentes","salvador","salvadoreno","tambien","ustedes","ustedes","mas","ano","anos","todos","todas","cada","uno","van","va","aqui","alla","estan","esta","estamos","vamos","solo","ahi","aqui","alla","aca","pais","nacional","gobierno","ademas","asi","tan","sino","traves","hecho","parte","hacia","desde","san","santa","quiero","quisiera","caso","gracias","ello","todo","toda","tres","uno","dos","fin","gran","hace","hacer","decir","tema","dias","forma","muchos","muchas","ver","pues","ser","dia","dar","cinco","temas","paises","ahora","mil","decia","partir","dado","area","millones","bonn","emuyshondt","ssestuyo","alcaldiass","alcalde","neto"))
docs <- tm_map(docs, removeWords, c("mencionarte", "verificamos","brindamos","espera","contactanos","favor","pendiente","seguimiento","poder","tardes","escribenos"))
docs <- tm_map(docs, removeWords, c("mencionarte", "verificamos"))
docs <- tm_map(docs, removePunctuation) #Quitamos signos de puntuacion
docs <- tm_map(docs, stripWhitespace) #Quitamos los espacios en blanco
dtm <- TermDocumentMatrix(docs) #construimos nuestra matriz
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v) # y estamos listos
head(d, 5) #Veamos el Top 5 de palabras
set.seed(1234) # Y estamos listos para generar nuestra nube de palabras
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=100, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
write.csv(d,"/Users/nelsonzepeda/Desktop/Datasphere/TextAnalytics-main/Stemming/TelcosSV/NUBE_CLARO_202006.csv", row.names = FALSE)


docs <- tm_map(docs, stemDocument, language="spanish") #Stemming
dtm <- TermDocumentMatrix(docs) #construimos nuestra matriz
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v) # y estamos listos
head(d, 5) #Veamos el Top 5 de palabras
set.seed(1234) # Y estamos listos para generar nuestra nube de palabras
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=100, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
write.csv(d,"/Users/nelsonzepeda/Desktop/Datasphere/TextAnalytics-main/Stemming/TelcosSV/NUBE_CLARO_STEM_202006.csv", row.names = FALSE)



