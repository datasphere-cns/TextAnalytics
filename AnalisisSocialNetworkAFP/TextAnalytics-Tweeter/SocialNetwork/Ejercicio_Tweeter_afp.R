library(rtweet)
library(tidyverse)
library(knitr)
library(tidyverse)
library(dplyr)
library(readr)
install.packages("tidytext")
install.packages("igraph")
install.packages("ggraph")

#-------------------------------------------------------------------------
##---- Cargamos los datos especificando los tipos de datos de cada columna 

##-AFP Conf?a
tweets_AFPConfia<- read_csv( "D:/Github/Datasphere/TextAnalytics/AnalisisSocialNetworkAFP/TextAnalytics-Tweeter/SocialNetwork/UTF8_AFPConfia.csv", 
                                   col_names = TRUE, col_types = cols(x1 = col_double(), 
                                                                      x2 = col_double(), 
                                                                      Name = col_character(),
                                                                      Username = col_character(),
                                                                      UserProfile = col_character(),
                                                                      TweetID  = col_character(),
                                                                      Retweets    = col_integer(),
                                                                      Comments    = col_integer(),
                                                                      Favorites     = col_integer(),
                                                                      `IsRetweet?`     =  col_character(),
                                                                      Date      =  col_character(),
                                                                      TweetText = col_character(),
                                                                      TweetSource = col_character()))

head(tweets_AFPConfia)
dim(tweets_AFPConfia)
str(tweets_AFPConfia)
colnames(tweets_AFPConfia)

## AFP Crecer
tweets_AFPCrecer<- read_csv( "D:/Github/Datasphere/TextAnalytics/AnalisisSocialNetworkAFP/TextAnalytics-Tweeter/SocialNetwork/UTF8_AFPCrecer.csv", 
                              col_names = TRUE, col_types = cols(x1 = col_double(), 
                                                                 x2 = col_double(), 
                                                                 Name = col_character(),
                                                                 Username = col_character(),
                                                                 UserProfile = col_character(),
                                                                 TweetID  = col_character(),
                                                                 Retweets    = col_integer(),
                                                                 Comments    = col_integer(),
                                                                 Favorites     = col_integer(),
                                                                 `IsRetweet?`     =  col_character(),
                                                                 Date      =  col_character(),
                                                                 TweetText = col_character(),
                                                                 TweetSource = col_character()))



head(tweets_AFPCrecer)
dim(tweets_AFPCrecer)
str(tweets_AFPCrecer)
colnames(tweets_AFPCrecer)

##- Para el campo "Date" es necesario hacer una transformaci?n para adecuar el formato 
tweets_AFPCrecer$Date <- as.Date(tweets_AFPCrecer$Date,format="%d/%m/%Y")
tweets_AFPCrecer$Date<- as.character(tweets_AFPCrecer$Date)

#-------------------------------------------------------------------------
##--Verificaci?n que los campos de todos los DF tengan el mismo nombre
colnames(tweets_AFPConfia)
colnames(tweets_AFPCrecer)

#-------------------------------------------------------------------------

#Agregamos una columnna que servir? para identificar los tweets de las diferentes empresas 
tweets_AFPCrecer <- cbind(tweets_AFPCrecer, account= c("AFPCrecer"))
tweets_AFPConfia <- cbind(tweets_AFPConfia, account= c("AFPConfia"))

#-------------------------------------------------------------------------

#Top 10 de usuario que m?s twitean respectivamente a cada cuenta de las que se est?n analizando
tweets_AFPCrecer %>% group_by(Username) %>% summarise(numero_tweets = n()) %>% top_n(10, numero_tweets) %>%  arrange(desc(numero_tweets)) 
tweets_AFPConfia %>% group_by(Username) %>% summarise(numero_tweets = n()) %>% top_n(10, numero_tweets) %>% arrange(desc(numero_tweets))


#-------------------------------------------------------------------------
# Unimos los DF  en un unico dataframe
tweetsbind <- bind_rows(tweets_AFPConfia,tweets_AFPCrecer)
tweetsbind %>% group_by(account) %>% summarise(numero_tweets = n()) 

head(tweetsbind)
dim(tweetsbind)
str(tweetsbind)
colnames(tweetsbind)


#-------------------------------------------------------------------------
# Selecci?n de variables
tweetsbind <- tweetsbind %>% select(Username, Date, TweetText, TweetSource , account, TweetSource)


#-------------------------------------------------------------------------

# Se renombran algunas  variables con nombres m?s pr?cticos
tweetsbind <- tweetsbind %>% rename(autor = Username, fecha = Date, texto = TweetText, empresa= account)
tweetsbind %>% group_by(empresa) %>% summarise(numero_tweets = n()) 


#-------------------------------------------------------------------------
#Excluir cuentas ra?z del community manager de cada empresa
tweetsbind <- tweetsbind[ !(tweetsbind$autor %in% c('AFPCrecer','AFPCONFIA' )), ]




#-------------------------------------------------------------------------
##Tokenizaci?n
#-------------------------------------------------------------------------

#Funcion para tokenizar el texto
limpiar_tokenizar <- function(texto){
  # El orden de la limpieza no es arbitrario
  # Se convierte todo el texto a min?sculas
  nuevo_texto <- tolower(texto)
  
  # Eliminaci?n de p?ginas web (palabras que empiezan por "http." seguidas de cualquier cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  
  # Eliminaci?n de signos de puntuaci?n
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  
  # Eliminaci?n de n?meros
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  
  # Eliminaci?n de espacios en blanco m?ltiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  
  #nuevo_texto <- str_replace_all(nuevo_texto,"[^[:graph:]]", " ") 
  
  # Tokenizaci?n por palabras individuales
  nuevo_texto <- str_split(nuevo_texto, " ")[[1]]
  
  # Eliminaci?n de tokens con una longitud < 2
  # nuevo_texto <- keep(.x = nuevo_texto, .p = function(x){str_length(x) > 1})
  return(nuevo_texto)
}


#-------------------------------------------------------------------------
# Se aplica la funci?n de limpieza y tokenizaci?n a cada tweet
tweetsbind <- tweetsbind %>% mutate(texto_tokenizado = map(.x = texto, .f = limpiar_tokenizar))

tweetsbind %>% select(texto_tokenizado) %>% head

tweetsbind %>% slice(1) %>% select(texto_tokenizado) %>% pull()


#Al realizar la tokenizaci?n, el elemento de estudio ha pasado a ser cada token (palabra)
#Debemos duplicar el valor de las otras columnas tantas veces como sea necesario. Ha este proceso se le conoce como expansi?n o unnest

tweets_tidy <- tweetsbind %>% select(-texto) %>% unnest()
tweets_tidy <- tweets_tidy %>% rename(token = texto_tokenizado)
head(tweets_tidy)


ggplot(tweetsbind, aes(x = as.Date(fecha), fill = empresa)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") +
  labs(x = "fecha de publicaci?n", y = "n?mero de tweets") +
  facet_wrap(~ empresa, ncol = 1) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))



#-------------------------------------------------------------------------
#Frecuencia de palabras

#Total de palabras utilizadas por cada usuario
tweets_tidy %>% group_by(empresa) %>% summarise(n = n())


tweets_tidy %>% ggplot(aes(x = empresa)) + geom_bar() + coord_flip() + theme_bw()


#-------------------------------------------------------------------------
#Palabras distintas utilizadas por cada usuario
tweets_tidy %>% select(empresa, token) %>% distinct() %>% group_by(empresa) %>%
  summarise(palabras_distintas = n())



tweets_tidy %>% select(empresa, token) %>% distinct() %>%
  ggplot(aes(x = empresa)) + geom_bar() + coord_flip() + theme_bw()


#-------------------------------------------------------------------------
#Palabras m?s utilizadas por usuario

tweets_tidy %>% group_by(empresa, token) %>% count(token) %>% group_by(empresa) %>%
  top_n(10, n) %>% arrange(empresa, desc(n)) %>% print(n=40)




#-------------------------------------------------------------------------
#Stop words
#Son preposiciones, pronombres., en general, palabras que no aportan informaci?n relevante sobre el texto

#lista_stopwords <- c('me', 'my', 'myself', 'we', 'our', 'ours', 'ourselves', 'que')
#lista_stopwords  <- data.frame(lista_stopwords)

lista_stopwords_esp <- read_csv("D:/Github/Datasphere/TextAnalytics/AnalisisSocialNetworkAFP/TextAnalytics-Tweeter/SocialNetwork/UTF8_spanish.csv", col_names = TRUE) 
lista_stopwords_esp <- as.vector(t(lista_stopwords_esp))

str(lista_stopwords_esp)

# Se filtran las stopwords
tweets_tidy <- tweets_tidy %>% filter(!(token %in% lista_stopwords_esp))

##--Excluir las cuentas oficiales 
tweets_tidy <- tweets_tidy[ !(tweets_tidy$token %in% c('afpconfia','afpcrecer','','$$','+','=')), ]

#Representaci?n gr?fica de las frecuencias
tweets_tidy %>% group_by(empresa, token) %>% count(token) %>% group_by(empresa) %>%
  top_n(10, n) %>% arrange(empresa, desc(n)) %>%
  ggplot(aes(x = reorder(token,n), y = n, fill = empresa)) +
  geom_col() +
  theme_bw() +
  labs(y = "", x = "") +
  theme(legend.position = "none") +
  coord_flip() +
  facet_wrap(~empresa,scales = "free", ncol = 1, drop = TRUE)

#-------------------------------------------------------------------------
#Word Clouds
#Otra forma visual de representar las palabras m?s frecuentes es mediante nubes de palabras


library(wordcloud)
library(RColorBrewer)

tweets_tidyAFPcrecer <- filter(tweets_tidy, empresa=='AFPCrecer')
tweets_tidyAFPconfia <- filter(tweets_tidy, empresa=='AFPConfia')

#-------------Nube de palabra para cada empresa
dfgrouped_AFPcrecer <-  tweets_tidyAFPcrecer %>%  group_by(token) %>% summarize(m = n()) %>%  ungroup()
head(dfgrouped_AFPcrecer)

wordcloud(words = dfgrouped_AFPcrecer$token, freq = dfgrouped_AFPcrecer$m, min.freq = 1, 
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


dfgrouped_AFPconfia <-  tweets_tidyAFPconfia %>%  group_by(token) %>% summarize(m = n()) %>%  ungroup()
wordcloud(words = dfgrouped_AFPconfia$token, freq = dfgrouped_AFPconfia$m, min.freq = 1, 
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#-------------------------------------------------------------------------
#Correlaci?n entre usuarios por palabras utilizadas
#Se usa el DF original cuando se hizo el bind tweets_tidy

library(gridExtra)
library(scales)

tweets_spread <- tweets_tidy %>% group_by(empresa, token) %>% count(token) %>%
  spread(key = empresa, value = n, fill = NA, drop = TRUE)

#--Correlacion entre 2 afp y 2 bancos
cor.test(~ AFPCrecer + AFPConfia, method = "pearson", data = tweets_spread)

p1 <- ggplot(tweets_spread, aes(AFPCrecer, AFPConfia)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())




grid.arrange(p1, nrow = 1)




#-------------------------------------------------------------------------
#Cantidad de palabras comunes entre AFP Crecer & AFP Conf?a
palabras_comunes <- dplyr::intersect(tweets_tidy %>% filter(empresa=="AFPCrecer") %>%
                                       select(token), tweets_tidy %>% filter(empresa=="AFPConfia") %>%
                                       select(token)) %>% nrow()
paste("N?mero de palabras comunes entre AFP Crecer & AFP Conf?a", palabras_comunes)


#----------------------------------------------------
#---- brigramas
#----- Se usa el Dataframe, que se carga al inicio 

install.packages("tidytext")
library(tidytext)

limpiar <- function(texto){
  # El orden de la limpieza no es arbitrario
  # Se convierte todo el texto a minúsculas
  nuevo_texto <- tolower(texto)
  
  # Eliminaci?n de p?ginas web (palabras que empiezan por "http." seguidas
  # de cualquier cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  
  # Eliminaci?n de signos de puntuaci?n
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  
  # Eliminaci?n de números
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  
  # Eliminaci?n de espacios en blanco m?ltiples
  #nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  return(nuevo_texto)
}

####-------------------------------------------
str(tweets_AFPCrecer)
dim(tweets_AFPCrecer)

# Selecci?n de variables
df_AFPCrecerbigrama <- tweets_AFPCrecer %>% select(Username, Date, TweetText, TweetSource , account, TweetSource)

# Se renombran algunas  variables con nombres m?s pr?cticos
df_AFPCrecerbigrama <- df_AFPCrecerbigrama %>% rename(autor = Username, fecha = Date, texto = TweetText, empresa= account)
df_AFPCrecerbigrama %>% group_by(empresa) %>% summarise(numero_tweets = n()) 



#Excluir cuentas ra?z
df_AFPCrecerbigrama <- df_AFPCrecerbigrama[ !(df_AFPCrecerbigrama$autor %in% c('AFPCrecer' )), ]

df_AFPCrecerbigrama %>% group_by(autor) %>% summarize(m = n()) %>%  ungroup()

##-----------------------------------------------------
#Creaci?n de bigramas de palabras

bigramasAFPCrecer <- df_AFPCrecerbigrama %>% mutate(texto = limpiar(texto)) %>%
  select(texto  ) %>% 
  unnest_tokens(input = texto  , output = "bigrama",
                token = "ngrams",n = 2, drop = TRUE)

# Contaje de ocurrencias de cada bigrama
bigramasAFPCrecer %>% count(bigrama, sort = TRUE)


#Los bigramas m?s frecuentes son los formados por stopwords. Como la relaci?n entre estas palabras no aporta informaci?n 
#de inter?s, se procede a eliminar todos aquellos bigramas que contienen alguna stopword.

##--Separaci?n de Bigramas
bigramas_separadosCrecer <- bigramasAFPCrecer %>% separate(bigrama, c("palabra1", "palabra2"),sep = " ")
head(bigramas_separadosCrecer)



# Filtrado de los bigramas que contienen alguna stopword
bigramas_separadosCrecer <- bigramas_separadosCrecer %>%
  filter(!palabra1 %in% lista_stopwords_esp) %>%
  filter(!palabra2 %in% lista_stopwords_esp)

# Uni?n de las palabras para formar de nuevo los bigramas
bigramasCrecer <- bigramas_separadosCrecer %>%
  unite(bigrama, palabra1, palabra2, sep = " ")

# Nuevo contaje para identificar los bigramas más frecuentes
bigramasCrecer %>% count(bigrama, sort = TRUE) %>% head(n = 10)


#-------------------------------------------------------------------------
#Una forma m?s visual e informativa de analizar las relaciones entre palabras es mediante el uso de networks. 

install.packages("igraph")
install.packages("ggraph")

library(igraph)
library(ggraph)

graph <- bigramasCrecer %>%
  separate(bigrama, c("palabra1", "palabra2"), sep = " ") %>%
  count(palabra1, palabra2, sort = TRUE) %>%
  filter(n > 3) %>% graph_from_data_frame(directed = FALSE)
set.seed(123)

plot(graph, vertex.label.font = 2,
     vertex.label.color = "black",
     vertex.label.cex = 0.7, edge.color = "gray85")







