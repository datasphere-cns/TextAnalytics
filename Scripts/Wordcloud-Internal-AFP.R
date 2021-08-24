install.packages("rlang")

library(rlang)
library(tidytext)
library(rtweet)
library(knitr)
library(tidyverse)
library(dplyr)
library(readr)
library(igraph)
library(ggraph)


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
head(DATASET_AFPCrecer)
dim(DATASET_AFPCrecer)


DATASET_AFPCrecer<-DATASET_AFPCrecer[complete.cases(DATASET_AFPCrecer[,]),]

head(DATASET_AFPCrecer)
str(DATASET_AFPCrecer)

DATASET_AFPCrecer <- cbind(DATASET_AFPCrecer, account= c("AFPCrecer"))
head(DATASET_AFPCrecer)

agg_area<-DATASET_AFPCrecer %>% group_by(Area) %>% summarise(numero_comentarios = n()) %>% top_n(10, numero_comentarios) %>%  arrange(desc(numero_comentarios)) 
agg_area
pie(agg_area$numero_comentarios, labels = agg_area$Area, main="Dist. Area")


agg_canal<-DATASET_AFPCrecer %>% group_by(Canal) %>% summarise(numero_comentarios = n()) %>% top_n(10, numero_comentarios) %>%  arrange(desc(numero_comentarios)) 


agg_canal

pie(agg_canal$numero_comentarios, labels = agg_canal$Canal, main="Dist. Canal")

comentsbind <-DATASET_AFPCrecer
comentsbind %>% group_by(account) %>% summarise(numero_comentarios = n()) 

comentsbind %>% group_by(Origen) %>% summarise(numero_comentarios = n()) 


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


comentsbind <- comentsbind %>% mutate(texto_tokenizado = map(.x = Mensaje, .f = limpiar_tokenizar))
comentsbind %>% select(texto_tokenizado) %>% head

comentsbind %>% slice(1) %>% select(texto_tokenizado) %>% pull()

coment_tidy <- comentsbind %>% select(-Mensaje) %>% unnest()
coment_tidy <- coment_tidy %>% rename(token = texto_tokenizado)
head(coment_tidy)

head(comentsbind)

comentsbind$Fecha <- as.Date(comentsbind$Fecha,format="%Y-%m-%d")



ggplot(comentsbind, aes(x = Fecha, fill = Canal)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  scale_x_date( date_breaks = "1 month") +
  labs(x = "fecha de publicacion", y = "numero de comentario") +
  facet_wrap(~ Canal, ncol = 1) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))



ggplot(comentsbind, aes(x = Fecha, fill = Area)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  scale_x_date( date_breaks = "1 month") +
  labs(x = "fecha de publicacion", y = "numero de comentario") +
  facet_wrap(~ Area, ncol = 1) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))


ggplot(comentsbind, aes(x = Fecha, fill = Area)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  scale_x_date( date_breaks = "1 month") +
  labs(x = "fecha de publicacion", y = "numero de comentario") +
  facet_wrap(~ Area, ncol = 1) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))



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
                                                       'quiero','quisiera','ahorrarescrecer','dias','años','datos','numero',
                                                       'nup','meses','trámite','tramite','consulta','proceso','hotmail',
                                                       'fecha','sacar','mes','dia','espero','ayudar','documentos','debo','quería','qieria',
                                                       'tardes','g','días','com','número','f','a','p','start','favor','cita','solicitar',
                                                       '<u+','nombre','dinero','dui','información','electrónico','c',
                                                       'recibido','>','abril','obtener','respuesta','mayo','esperar','tarjeta','s','tarde',
                                                       'enviar','envíe','aun','persona','medio','envié','><u+','actualizar','ayuda','pregunta',
                                                       'ana','antonio','recibí','aser','junio','señores','josé','electronico'
                                                       ,'porfavor','queria',
                                                       'amable','yahoo','deseo','envío','m','pedir','ayudame','recibir','sa',
                                                       'k','d','x','noches','pensión','pension','gracias','necesito')), ]

#Representaci?n gr?fica de las frecuencias
coment_tidy %>% group_by(Canal, token) %>% count(token) %>% group_by(Canal) %>%
  top_n(10, n) %>% arrange(Canal, desc(n)) %>%
  ggplot(aes(x = reorder(token,n), y = n, fill = Canal)) +
  geom_col() +
  theme_bw() +
  labs(y = "", x = "") +
  theme(legend.position = "none") +
  coord_flip() +
  facet_wrap(~Canal,scales = "free", ncol = 1, drop = TRUE)




#-------------------------------------------------------------------------
#Word Clouds
#Otra forma visual de representar las palabras m?s frecuentes es mediante nubes de palabras


library(wordcloud)
library(RColorBrewer)


#-------------Nube de palabra para cada empresa
dfgrouped_AFPcrecer <-  coment_tidy %>%  group_by(token) %>% summarize(m = n()) %>%  ungroup()
head(dfgrouped_AFPcrecer)

wordcloud(words = dfgrouped_AFPcrecer$token, freq = dfgrouped_AFPcrecer$m, min.freq = 20, 
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))





# Selecci?n de variables
df_AFPCrecerbigrama <- DATASET_AFPCrecer %>% select(Id, Fecha, Mensaje, Canal)

df_AFPCrecerbigrama %>% group_by(Canal) %>% summarise(numero_tweets = n()) 






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

#Creaci?n de bigramas de palabras

bigramasAFPCrecer <- df_AFPCrecerbigrama %>% mutate(Mensaje = limpiar(Mensaje)) %>%
  select(Mensaje  ) %>% 
  unnest_tokens(input = Mensaje  , output = "bigrama",
                token = "ngrams",n = 2, drop = TRUE)

# Contaje de ocurrencias de cada bigrama
bigramasAFPCrecer %>% count(bigrama, sort = TRUE)


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


graph <- bigramasCrecer %>%
  separate(bigrama, c("palabra1", "palabra2"), sep = " ") %>%
  count(palabra1, palabra2, sort = TRUE) %>%
  filter(n > 400) %>% graph_from_data_frame(directed = FALSE)
set.seed(123)

plot(graph, vertex.label.font = 2,
     vertex.label.color = "black",
     vertex.label.cex = 0.9, edge.color = "gray25")
