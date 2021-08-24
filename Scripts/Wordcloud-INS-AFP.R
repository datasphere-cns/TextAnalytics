remove.packages("rlang")
install.packages("rlang")
library(rlang)
install.packages("tidytext")
library(tidytext)
library(rtweet)
library(tidyverse)
library(knitr)
library(tidyverse)
library(dplyr)
library(readr)

install.packages("igraph")
install.packages("ggraph")

library(igraph)
library(ggraph)




#-------------------------------------------------------------------------
##---- Cargamos los datos especificando los tipos de datos de cada columna 


## AFP Crecer
DATASET_AFPCrecer<- read_csv( "C:/Users/nelso/Downloads/AWS/AWS/NLP/Reports/IN-ENE-JUL.csv", 
                               col_names = TRUE, col_types = cols(
                                                                  Name = col_character(),
                                                                  ProfileID = col_character(),
                                                                  Date = col_character(),
                                                                  Comment    = col_character(),
                                                                  Sentiment   = col_character(),
                                                                  Mes   = col_character(),
                                                                  RS   = col_character()
                                                                  ))



head(DATASET_AFPCrecer)
dim(DATASET_AFPCrecer)
str(DATASET_AFPCrecer)
colnames(DATASET_AFPCrecer)

##- Para el campo "Date" es necesario hacer una transformaci?n para adecuar el formato 
DATASET_AFPCrecer$Date <- as.Date(DATASET_AFPCrecer$Date,format="%m/%d/%Y")
##DATASET_AFPCrecer$Date<- as.character(DATASET_AFPCrecer$Date)




#-------------------------------------------------------------------------
##--Verificaci?n que los campos de todos los DF tengan el mismo nombre
colnames(DATASET_AFPCrecer)

#-------------------------------------------------------------------------

#Agregamos una columnna que servir? para identificar los tweets de las diferentes empresas 
DATASET_AFPCrecer <- cbind(DATASET_AFPCrecer, account= c("AFPCrecer"))

#-------------------------------------------------------------------------

#Top 10 de usuario que m?s twitean respectivamente a cada cuenta de las que se est?n analizando
DATASET_AFPCrecer %>% group_by(Name) %>% summarise(numero_comentarios = n()) %>% top_n(10, numero_comentarios) %>%  arrange(desc(numero_comentarios)) 


#-------------------------------------------------------------------------
# Unimos los DF  en un unico dataframe

#comentsbind <- bind_rows(DATASET_AFPConfia,DATASET_AFPCrecer)
comentsbind <-DATASET_AFPCrecer
comentsbind %>% group_by(account) %>% summarise(numero_comentarios = n()) 

head(comentsbind)
dim(comentsbind)
str(comentsbind)
colnames(comentsbind)


#-------------------------------------------------------------------------
# Selecci?n de variables
comentsbind <- comentsbind %>% select(Name, Date, Comment, account)


#-------------------------------------------------------------------------

# Se renombran algunas  variables con nombres m?s pr?cticos
comentsbind <- comentsbind %>% rename(autor = Name, fecha = Date, texto = Comment, empresa= account)
comentsbind %>% group_by(empresa) %>% summarise(numero_comentarios = n()) 


#-------------------------------------------------------------------------
#Excluir cuentas ra?z del community manager de cada empresa
comentsbind <- comentsbind[ !(comentsbind$autor %in% c('afpcrecersv','AFPCONFIA' )), ]

dim(comentsbind)

## 

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
# Se aplica la funcion de limpieza y tokenizaci?n a cada tweet
test = "Este es 120 ejempl0 de l'limpieza de6 TEXTO  https://t.co/rnHPgyhx4Z @JoaquinAmatRodrigo #textmining"
limpiar_tokenizar(texto = test)

comentsbind <- comentsbind %>% mutate(texto_tokenizado = map(.x = texto, .f = limpiar_tokenizar))
#rlang::last_error()
comentsbind %>% select(texto_tokenizado) %>% head

comentsbind %>% slice(1) %>% select(texto_tokenizado) %>% pull()
head(comentsbind)

#Al realizar la tokenizaci?n, el elemento de estudio ha pasado a ser cada token (palabra)
#Debemos duplicar el valor de las otras columnas tantas veces como sea necesario. Ha este proceso se le conoce como expansi?n o unnest

coment_tidy <- comentsbind %>% select(-texto) %>% unnest()
coment_tidy <- coment_tidy %>% rename(token = texto_tokenizado)
head(coment_tidy)


ggplot(comentsbind, aes(x = fecha, fill = empresa)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  scale_x_date( date_breaks = "1 month") +
  labs(x = "fecha de publicacion", y = "numero de comentario") +
  facet_wrap(~ empresa, ncol = 1) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))



#-------------------------------------------------------------------------
#Frecuencia de palabras

#Total de palabras utilizadas por cada usuario
coment_tidy %>% group_by(empresa) %>% summarise(n = n())


coment_tidy %>% ggplot(aes(x = empresa)) + geom_bar() + coord_flip() + theme_bw()


#-------------------------------------------------------------------------
#Palabras distintas utilizadas por cada usuario
coment_tidy %>% select(empresa, token) %>% distinct() %>% group_by(empresa) %>%
  summarise(palabras_distintas = n())



coment_tidy %>% select(empresa, token) %>% distinct() %>%
  ggplot(aes(x = empresa)) + geom_bar() + coord_flip() + theme_bw()


#-------------------------------------------------------------------------
#Palabras m?s utilizadas por usuario

coment_tidy %>% group_by(empresa, token) %>% count(token) %>% group_by(empresa) %>%
  top_n(10, n) %>% arrange(empresa, desc(n)) %>% print(n=40)




#-------------------------------------------------------------------------
#Stop words
#Son preposiciones, pronombres., en general, palabras que no aportan informaci?n relevante sobre el texto

#lista_stopwords <- c('me', 'my', 'myself', 'we', 'our', 'ours', 'ourselves', 'que')
#lista_stopwords  <- data.frame(lista_stopwords)

lista_stopwords_esp <- read_csv("C:/AFP-TextAnalysis/Training/UTF8_spanish.csv", col_names = TRUE) 
lista_stopwords_esp <- as.vector(t(lista_stopwords_esp))

str(lista_stopwords_esp)

# Se filtran las stopwords
coment_tidy <- coment_tidy %>% filter(!(token %in% lista_stopwords_esp))

##--Excluir las cuentas oficiales 
coment_tidy <- coment_tidy[ !(coment_tidy$token %in% c('afpconfia','afpcrecer','','$$','+','=','afp',
                                                       'crecer','confia','ok','hola','ustedes','saldo','pension','día',
                                                       'quiero','quisiera','ahorrarescrecer','dias','años','tardes','g','días',
                                                       'k','d','x','noches','pensión','pension','gracias',
                                                       'mande','caso','marzo','israelflores','disculpe','ana'
                                                       ,'afpcrecersv')), ]

#Representaci?n gr?fica de las frecuencias
coment_tidy %>% group_by(empresa, token) %>% count(token) %>% group_by(empresa) %>%
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
coment_tidy
coments_tidyAFPcrecer <- filter(coment_tidy, empresa=='AFPCrecer')
coments_tidyAFPconfia <- filter(coment_tidy, empresa=='AFPConfia')

#-------------Nube de palabra para cada empresa
dfgrouped_AFPcrecer <-  coments_tidyAFPcrecer %>%  group_by(token) %>% summarize(m = n()) %>%  ungroup()
head(dfgrouped_AFPcrecer)

wordcloud(words = dfgrouped_AFPcrecer$token, freq = dfgrouped_AFPcrecer$m, min.freq = 3, 
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))



#-------------Nube de palabra para cada empresa
dfgrouped_AFPcrecer <-  coments_tidyAFPcrecer %>%  group_by(token) %>% summarize(m = n()) %>%  ungroup()
head(dfgrouped_AFPcrecer)

wordcloud(words = dfgrouped_AFPcrecer$token, freq = dfgrouped_AFPcrecer$m, min.freq = 30, 
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#-------------------------------------------------------------------------
#Correlaci?n entre usuarios por palabras utilizadas
#Se usa el DF original cuando se hizo el bind tweets_tidy

library(gridExtra)
library(scales)

coment_spread <- coment_tidy %>% group_by(empresa, token) %>% count(token) %>%
  spread(key = empresa, value = n, fill = NA, drop = TRUE)



#-------------------------------------------------------------------------
#----------------------------------------------------
#---- brigramas
#----- Se usa el Dataframe, que se carga al inicio 


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
str(DATASET_AFPCrecer)
dim(DATASET_AFPCrecer)

# Selecci?n de variables
df_AFPCrecerbigrama <- DATASET_AFPCrecer %>% select(Name, Date, Comment, account)

# Se renombran algunas  variables con nombres m?s pr?cticos
df_AFPCrecerbigrama <- df_AFPCrecerbigrama %>% rename(autor = Name, fecha = Date, texto = Comment, empresa= account)
df_AFPCrecerbigrama %>% group_by(empresa) %>% summarise(numero_tweets = n()) 




#Excluir cuentas ra?z
df_AFPCrecerbigrama <- df_AFPCrecerbigrama[ !(df_AFPCrecerbigrama$autor %in% c('afpcrecersv' )), ]

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



graph <- bigramasCrecer %>%
  separate(bigrama, c("palabra1", "palabra2"), sep = " ") %>%
  count(palabra1, palabra2, sort = TRUE) %>%
  filter(n > 1) %>% graph_from_data_frame(directed = FALSE)
set.seed(123)

plot(graph, vertex.label.font = 2,
     vertex.label.color = "black",
     vertex.label.cex = 0.9, edge.color = "gray25")
