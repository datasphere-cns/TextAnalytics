install.packages("readr")
install.packages("xlsx")
library(rtweet)
library(tidyverse)
library(knitr)
library(tidyverse)
library(dplyr)
library(readr)


#Cargamos los datasets en dataFrames respectivamente
tweets_AFPConfia <- read_csv("D:/Github/Datasphere/TextAnalytics/SocialNetwork/twitter_AfpConfia.csv", col_names = TRUE)
tweets_AFPCrecer <- read_csv("D:/Github/Datasphere/TextAnalytics/SocialNetwork/twitter_AfpCrecer.csv",col_names = TRUE)
tweets_BACCredomatic <- read_csv( "D:/Github/Datasphere/TextAnalytics/SocialNetwork/twitter_BACCredomatic.csv", col_names = TRUE)
tweets_Cuscatlan <- read_csv( "D:/Github/Datasphere/TextAnalytics/SocialNetwork/twitter_Cuscatlan.csv", col_names = TRUE)


colnames(tweets_AFPConfia)
colnames(tweets_AFPCrecer)
colnames(tweets_BACCredomatic)
colnames(tweets_Cuscatlan)

dim(tweets_AFPConfia)
.
dim(tweets_BACCredomatic)
dim(tweets_Cuscatlan)


head(tweets_AFPConfia)
head(tweets_AFPCrecer)
head(tweets_BACCredomatic)
head(tweets_Cuscatlan)



# Unimos los tweets en un Ãºnico dataframe
tweets2 <- bind_rows(tweets_AFPConfia,tweets_AFPCrecer, tweets_BACCredomatic, tweets_Cuscatlan)
tweets2 %>% group_by(Username) %>% summarise(numero_tweets = n())


# Aplicando transformaciones.
tweets_AFPConfia <- read_csv("D:/Github/Datasphere/TextAnalytics/SocialNetwork/AfpConfiaUTF8.csv", 
                             col_names = TRUE, col_types = cols(X1 = col_double(), 
                                                                X2 = col_double(), 
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

str(tweets_AFPConfia)
head(tweets_AFPConfia)
tweets_AFPConfia$TweetText <- iconv(tweets_AFPConfia$TweetText,from="UTF-8",to="ASCII//TRANSLIT")

head(tweets_AFPConfia2)
str(tweets_AFPConfia2)


tweets_AFPCrecer <- read_csv("D:/Github/Datasphere/TextAnalytics/SocialNetwork/CrecerUTF8.csv",  
                             col_names = TRUE, col_types = cols(X1 = col_double(), 
                                                                X2 = col_double(), 
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
                                                                TweetSource = col_character()) )



tweets_AFPCrecer$Date <- as.Date(tweets_AFPCrecer$Date,format="%d/%m/%Y")
head(tweets_AFPCrecer)
str(tweets_AFPCrecer)

tweets_AFPCrecer$Date<- as.character(tweets_AFPCrecer$Date)



tweets_BACCredomatic <- read_csv( "D:/Github/Datasphere/TextAnalytics/SocialNetwork/BACCredomaticUTF8.csv", 
                                  col_names = TRUE, col_types = cols(X1 = col_double(), 
                                                                     X2 = col_double(), 
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


tweets_BACCredomatic$Date <- as.Date(tweets_BACCredomatic$Date,format="%d/%m/%Y")
head(tweets_BACCredomatic)
str(tweets_BACCredomatic)

tweets_Cuscatlan <- read_csv( "D:/Github/Datasphere/TextAnalytics/SocialNetwork/CuscatlanUTF8.csv", 
                              col_names = TRUE, col_types = cols(X1 = col_double(), 
                                                                 X2 = col_double(), 
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


str(tweets_Cuscatlan)


tweets_AFPConfia$Date <- as.Date(tweets_AFPConfia$Date,format="%d/%m/%Y")

tweets_BACCredomatic$Date <- as.Date(tweets_BACCredomatic$Date,format="%d/%m/%Y")
tweets_Cuscatlan$Date <- as.Date(tweets_Cuscatlan$Date,format="%d/%m/%Y")

#ValidaciÃ³n
head(tweets_AFPConfia)
head(tweets_AFPCrecer)
head(tweets_BACCredomatic)
head(tweets_Cuscatlan)

str(tweets_BACCredomatic)
str(tweets_AFPCrecer)
tweets_BACCredomatic

#Totalizar la cantidad de tweets por cuenta
tweets_AFPCrecer %>%  summarise(numero_tweets = n())
tweets_AFPConfia %>% summarise(numero_tweets = n())
tweets_BACCredomatic %>% summarise(numero_tweets = n())
tweets_Cuscatlan %>%  summarise(numero_tweets = n())


#Top 10 de usuario que mÃ¡s twitean respectivamente a cada cuenta de las que se estÃ¡n analizando
tweets_AFPCrecer %>% group_by(Username) %>% summarise(numero_tweets = n()) %>% top_n(15, numero_tweets) %>%  arrange(desc(numero_tweets)) 
tweets_AFPConfia %>% group_by(Username) %>% summarise(numero_tweets = n()) %>% top_n(20, numero_tweets) %>% arrange(desc(numero_tweets))
tweets_BACCredomatic %>% group_by(Username) %>% summarise(numero_tweets = n()) %>% top_n(20, numero_tweets) %>% arrange(desc(numero_tweets))
tweets_Cuscatlan %>% group_by(Username) %>% summarise(numero_tweets = n()) %>% top_n(20, numero_tweets) %>% arrange(desc(numero_tweets))

tweets_AFPCrecer <- tweets_AFPCrecer[ !(tweets_AFPCrecer$Username %in% c('AFPCrecer')), ]

str(tweets_AFPCrecer)

#Agregamos una columnna que servirÃ¡ para identificar los tweets de las diferentes empresas 
tweets_AFPCrecer <- cbind(tweets_AFPCrecer, account= c("AFPCrecer"))
tweets_Cuscatlan <- cbind(tweets_Cuscatlan, account= c("Cuscatlan"))
tweets_BACCredomatic <- cbind(tweets_BACCredomatic, account= c("BacCredomatic"))
tweets_AFPConfia <- cbind(tweets_AFPConfia, account= c("AFPConfia"))



str(tweets_AFPConfia)
str(tweets_AFPCrecer)
str(tweets_BACCredomatic)
str(tweets_Cuscatlan)

head(tweets_AFPCrecer)

# Unimos los tweets en un unico dataframe
tweetsbind <- bind_rows(tweets_AFPConfia,tweets_AFPCrecer, tweets_BACCredomatic, tweets_Cuscatlan)
tweetsbind %>% group_by(account) %>% summarise(numero_tweets = n()) 
head(tweetsbind)
dim(tweetsbind)
str(tweetsbind)
colnames(tweetsbind)

subset(tweetsbind, account== "AFPConfia")



# SelecciÃ³n de variables
tweetsbind <- tweetsbind %>% select(Username, Date, TweetText, TweetSource , account, TweetSource)

# Se renombran algunas  variables con nombres mÃ¡s prÃ¡cticos
tweetsbind <- tweetsbind %>% rename(autor = Username, fecha = Date, texto = TweetText, empresa= account)
tweetsbind %>% group_by(empresa) %>% summarise(numero_tweets = n()) 

head(tweetsbind)

tweetsbind <- tweetsbind[ !(tweetsbind$autor %in% c('AFPCrecer', 'BancoCUSCATLAN_','BACCredomaticSV','AFPCONFIA' )), ]






#------------------------------
#Funcion para tokenizar el texto
limpiar_tokenizar <- function(texto){
  

  #iconv(texto, to='ASCII//TRANSLIT')

  
  # Se convierte todo el texto a min?sculas
  nuevo_texto <- tolower(texto)
  
  # El orden de la limpieza no es arbitrario
 
  
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

tweetsbind



test2 = "Realiza tus trámites ingresando a #ConfíaVirtual.Si todavía no has registrado tu E-mail"
test = "Este es 120 ejempl0 de l'límpieza de6 TEXTO  https://t.co/rnHPgyhx4Z @JoaquinAmatRodrigo #textmining"
limpiar_tokenizar(texto = test2)

# Se aplica la función de limpieza y tokenizaci?n a cada tweet
tweetsbind <- tweetsbind %>% mutate(texto_tokenizado = map(.x = texto, .f = limpiar_tokenizar))

tweetsbind %>% select(texto_tokenizado) %>% head

tweetsbind %>% slice(1) %>% select(texto_tokenizado) %>% pull()


tweets_tidy <- tweetsbind %>% select(-texto) %>% unnest()
tweets_tidy <- tweets_tidy %>% rename(token = texto_tokenizado)
head(tweets_tidy)




ggplot(tweetsbind, aes(x = as.Date(fecha), fill = empresa)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") +
  labs(x = "fecha de publicación", y = "número de tweets") +
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
#Palabras más utilizadas por usuario

tweets_tidy %>% group_by(empresa, token) %>% count(token) %>% group_by(empresa) %>%
  top_n(15, n) %>% arrange(empresa, desc(n)) %>% print(n=45)

subset(tweets_tidy, empresa== "AFPCrecer")


