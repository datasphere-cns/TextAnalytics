install.packages('rtweet')
install.packages('tidyverse')
install.packages('knitr')
install.packages("readr")
install.packages("dplyr")  
install.packages("gridExtra") 
install.packages("scales") 
install.packages("scales")

library(rtweet)
library(tidyverse)
library(knitr)
library(tidyverse)
library(dplyr)


tweets_elon <- read_csv("D:/Workshop DataSphere/Data Profiling en Lenguaje R/Ejemplos/datos_tweets_@elonmusk.csv", col_names = TRUE)
tweets_BillGates <- read_csv(file = "D:/Workshop DataSphere/Data Profiling en Lenguaje R/Ejemplos/datos_tweets_@BillGates.csv",col_names = TRUE)
tweets_mayoredlee <- read_csv( "D:/Workshop DataSphere/Data Profiling en Lenguaje R/Ejemplos/datos_tweets_@mayoredlee.csv", col_names = TRUE)

# Unimos los tweets en un único dataframe
tweets <- bind_rows(tweets_elon, tweets_BillGates, tweets_mayoredlee)
tweets %>% group_by(screen_name) %>% summarise(numero_tweets = n())

#Listado de columas, vamos a determinar cuales vamos a usar para nuestro análisis.
colnames(tweets)

# Selección de variables
tweets <- tweets %>% select(screen_name, created_at, status_id, text)

# Se renombran las variables con nombres más prácticos
tweets <- tweets %>% rename(autor = screen_name, fecha = created_at, texto = text, tweet_id = status_id)

#Validar los cambios

head(tweets)



##Limpieza de texto y tokenización
#El proceso de limpieza de texto, dentro del ámbito de text mining, consiste en eliminar del 
#texto todo aquello que no aporte información sobre su temática, estructura o contenido

#Patrones no informativos (urls de páginas web)
#Signos de puntuación
#Etiquetas HTML
#Caracteres sueltos
#Números

#Funcion para tokenizar el texto
limpiar_tokenizar <- function(texto){
  
  # El orden de la limpieza no es arbitrario
  
  # Se convierte todo el texto a minúsculas
  nuevo_texto <- tolower(texto)
  
  
  # Eliminación de páginas web (palabras que empiezan por "http." seguidas de cualquier cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  
  # Eliminación de signos de puntuación
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  
  # Eliminación de números
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  
  # Eliminación de espacios en blanco múltiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  
  # Tokenización por palabras individuales
  nuevo_texto <- str_split(nuevo_texto, " ")[[1]]
  
  # Eliminación de tokens con una longitud < 2
  nuevo_texto <- keep(.x = nuevo_texto, .p = function(x){str_length(x) > 1})
  return(nuevo_texto)
}

test = "Este es 120 ejempl0 de l'limpieza de6 TEXTO  https://t.co/rnHPgyhx4Z @JoaquinAmatRodrigo #textmining"
limpiar_tokenizar(texto = test)

# Se aplica la función de limpieza y tokenización a cada tweet
tweets <- tweets %>% mutate(texto_tokenizado = map(.x = texto, .f = limpiar_tokenizar))
tweets %>% select(texto_tokenizado) %>% head()

#Gracias a la característica de las tibble de poder contener cualquier tipo de elemento en sus
#columnas (siempre que sea el mismo para toda la columna), se puede almacenar el texto
#tokenizado. Cada elemento de la columna texto_tokenizado es una lista con un vector de tipo
#character que contiene los tokens generados


tweets %>% slice(1) %>% select(texto_tokenizado) %>% pull()

tweets
#Análisis exploratorio de Datos - EDA 

#En R, las estructuras por excelencia para el análisis exploratorio son el DataFrame y la
#Tibble, que es la forma en la que se encuentra almacenada ahora la información de los tweets.

#Previamente a la división del texto, los elementos de estudio (observaciones) eran los tweets, y cada uno se
#encontraba en una fila, cumplimento así la condición de tidy data: una observación, una fila

#Al realizar la tokenización, el elemento de estudio ha pasado a ser cada token (palabra)
#Debemos duplicar el valor de las otras columnas tantas veces como sea necesario. Ha este proceso se le conoce como expansión o unnest

tweets_tidy <- tweets %>% select(-texto) %>% unnest()
tweets_tidy <- tweets_tidy %>% rename(token = texto_tokenizado)
head(tweets_tidy)

#Distribución temporal de los tweets
install.packages("lubridate")  
library(lubridate)

#Dado que cada usuario puede haber iniciado su actividad en Twitter en diferente
#momento, es interesante explorar si los tweets recuperados solapan en el tiempo

ggplot(tweets, aes(x = as.Date(fecha), fill = autor)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "5 month") +
  labs(x = "fecha de publicación", y = "número de tweets") +
  facet_wrap(~ autor, ncol = 1) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))



tweets_mes_anyo <- tweets %>% mutate(mes_anyo = format(fecha, "%Y-%m"))
tweets_mes_anyo %>% group_by(autor, mes_anyo) %>% summarise(n = n()) %>%
  ggplot(aes(x = mes_anyo, y = n, color = autor)) +
  geom_line(aes(group = autor)) +
  labs(title = "Número de tweets publicados", x = "fecha de publicación",
       y = "número de tweets") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 6),
        legend.position = "bottom")


#Frecuencia de palabras

#Total de palabras utilizadas por cada usuario
tweets_tidy %>% group_by(autor) %>% summarise(n = n())


tweets_tidy %>% ggplot(aes(x = autor)) + geom_bar() + coord_flip() + theme_bw()


#Palabras distintas utilizadas por cada usuario
tweets_tidy %>% select(autor, token) %>% distinct() %>% group_by(autor) %>%
  summarise(palabras_distintas = n())



tweets_tidy %>% select(autor, token) %>% distinct() %>%
  ggplot(aes(x = autor)) + geom_bar() + coord_flip() + theme_bw()

tweets_tidy
#Longitud media de los tweets por usuario
tweets_tidy %>% group_by(autor, tweet_id) %>% summarise(longitud = n()) %>%
  group_by(autor) %>% summarise(media_longitud = mean(longitud),
                                sd_longitud = sd(longitud))


tweets_tidy %>% group_by(autor, tweet_id) %>% summarise(longitud = n()) %>%
  group_by(autor) %>%
  summarise(media_longitud = mean(longitud),
            sd_longitud = sd(longitud)) %>%
  ggplot(aes(x = autor, y = media_longitud)) +
  geom_col() +
  geom_errorbar(aes(ymin = media_longitud - sd_longitud,
                    ymax = media_longitud + sd_longitud)) +
  coord_flip() + theme_bw()


#Palabras más utilizadas por usuario

tweets_tidy %>% group_by(autor, token) %>% count(token) %>% group_by(autor) %>%
  top_n(15, n) %>% arrange(autor, desc(n)) %>% print(n=45)


#Stop words
#Son preposiciones, pronombres., en general, palabras que no aportan información relevante sobre el texto



lista_stopwords <- c('me', 'my', 'myself', 'we', 'our', 'ours', 'ourselves',
                     'you','your', 'yours', 'yourself', 'yourselves', 'he', 'him','his',
                     'himself', 'she', 'her', 'hers', 'herself', 'it', 'its', 'itself',
                     'they', 'them', 'their', 'theirs', 'themselves', 'what', 'which',
                     'who', 'whom', 'this', 'that', 'these', 'those', 'am', 'is', 'are',
                     'was', 'were', 'be', 'been', 'being', 'have', 'has', 'had',
                     'having', 'do', 'does', 'did', 'doing', 'a', 'an', 'the', 'and',
                     'but', 'if', 'or', 'because', 'as', 'until', 'while', 'of', 'at',
                     'by', 'for', 'with', 'about', 'against', 'between', 'into',
                     'through', 'during', 'before', 'after', 'above', 'below', 'to',
                     'from', 'up', 'down', 'in', 'out', 'on', 'off', 'over', 'under',
                     'again', 'further', 'then', 'once', 'here', 'there', 'when',
                     'where', 'why', 'how', 'all', 'any', 'both', 'each', 'few', 'more',
                     'most', 'other', 'some', 'such', 'no', 'nor', 'not', 'only', 'own',
                     'same', 'so', 'than', 'too', 'very', 's', 't', 'can', 'will',
                     'just', 'don', 'should', 'now', 'd', 'll', 'm', 'o', 're', 've',
                     'y', 'ain', 'aren', 'couldn', 'didn', 'doesn', 'hadn', 'hasn',
                     'haven', 'isn', 'ma', 'mightn', 'mustn', 'needn', 'shan',
                     'shouldn', 'wasn', 'weren', 'won', 'wouldn','i')


#En la tabla anterior aparece el término amp que procede de la etiqueta html &amp
# Se añade el término amp al listado de stopwords
lista_stopwords <- c(lista_stopwords, "amp")

# Se filtran las stopwords
tweets_tidy <- tweets_tidy %>% filter(!(token %in% lista_stopwords))

#Representación gráfica de las frecuencias
tweets_tidy %>% group_by(autor, token) %>% count(token) %>% group_by(autor) %>%
  top_n(10, n) %>% arrange(autor, desc(n)) %>%
  ggplot(aes(x = reorder(token,n), y = n, fill = autor)) +
  geom_col() +
  theme_bw() +
  labs(y = "", x = "") +
  theme(legend.position = "none") +
  coord_flip() +
  facet_wrap(~autor,scales = "free", ncol = 1, drop = TRUE)


#Word Clouds
#Otra forma visual de representar las palabras más frecuentes es mediante nubes de palabras


library(wordcloud)
library(RColorBrewer)

wordcloud_custom <- function(grupo, df){
  print(grupo)
  wordcloud(words = df$token, freq = df$frecuencia,
            max.words = 400, random.order = FALSE, rot.per = 0.35,
            colors = brewer.pal(8, "Dark2"))
}

df_grouped <- tweets_tidy %>% group_by(autor, token) %>% count(token) %>%
  group_by(autor) %>% mutate(frecuencia = n / n()) %>%
  arrange(autor, desc(frecuencia)) %>% nest()

walk2(.x = df_grouped$autor, .y = df_grouped$data, .f = wordcloud_custom)


#Correlación entre usuarios por palabras utilizadas

library(gridExtra)
library(scales)
tweets_spread <- tweets_tidy %>% group_by(autor, token) %>% count(token) %>%
  spread(key = autor, value = n, fill = NA, drop = TRUE)


cor.test(~ mayoredlee + elonmusk, method = "pearson", data = tweets_spread)
cor.test(~ BillGates + elonmusk, method = "pearson", data = tweets_spread)


p1 <- ggplot(tweets_spread, aes(elonmusk, mayoredlee)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())


p2 <- ggplot(tweets_spread, aes(elonmusk, BillGates)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

grid.arrange(p1, p2, nrow = 1)


#Cantidad de palabras comunes entre Elon Musk & Ed Lee
palabras_comunes <- dplyr::intersect(tweets_tidy %>% filter(autor=="elonmusk") %>%
                                       select(token), tweets_tidy %>% filter(autor=="mayoredlee") %>%
                                       select(token)) %>% nrow()
paste("Número de palabras comunes entre Elon Musk y Ed Lee", palabras_comunes)

#Cantidad de palabras comunes entre Elon Musk & Bill Gates
palabras_comunes <- dplyr::intersect(tweets_tidy %>% filter(autor=="elonmusk") %>%
                                       select(token), tweets_tidy %>% filter(autor=="BillGates") %>%
                                       select(token)) %>% nrow()
paste("Número de palabras comunes entre Elon Musk y Bill Gates", palabras_comunes)

----------------------------------
#Comparación en el uso de palabras

# Pivotaje y despivotaje
tweets_spread <- tweets_tidy %>% group_by(autor, token) %>% count(token) %>%
  spread(key = autor, value = n, fill = 0, drop = TRUE)
tweets_unpivot <- tweets_spread %>% gather(key = "autor", value = "n", -token)



# Selección de los autores elonmusk y mayoredlee
tweets_unpivot <- tweets_unpivot %>% filter(autor %in% c("elonmusk",
                                                         "mayoredlee"))
# Se añade el total de palabras de cada autor
tweets_unpivot <- tweets_unpivot %>% left_join(tweets_tidy %>%
                                                 group_by(autor) %>%
                                                 summarise(N = n()),
                                               by = "autor")



# Cálculo de odds y log of odds de cada palabra
tweets_logOdds <- tweets_unpivot %>% mutate(odds = (n + 1) / (N + 1))
tweets_logOdds <- tweets_logOdds %>% select(autor, token, odds) %>%
  spread(key = autor, value = odds)
tweets_logOdds <- tweets_logOdds %>% mutate(log_odds = log(elonmusk/mayoredlee),
                                            abs_log_odds = abs(log_odds))



# Si el logaritmo de odds es mayor que cero, significa que es una palabra con
# mayor probabilidad de ser de Elon Musk. Esto es así porque el ratio sea ha
# calculado como elonmusk/mayoredlee.
tweets_logOdds <- tweets_logOdds %>%
  mutate(autor_frecuente = if_else(log_odds > 0,
                                   "@elonmusk",
                                   "@mayoredlee"))
tweets_logOdds %>% arrange(desc(abs_log_odds)) %>% head()




#Representación de las 30 palabras más diferenciadas
tweets_logOdds %>% group_by(autor_frecuente) %>% top_n(15, abs_log_odds) %>%
  ggplot(aes(x = reorder(token, log_odds), y = log_odds, fill = autor_frecuente)) +
  geom_col() +
  labs(x = "palabra", y = "log odds ratio (@elonmusk / mayoredlee)") +
  coord_flip() +
  theme_bw()




#Relación entre palabras
#En todos los análisis anteriores, se han considerado a las palabras como unidades
#individuales e independientes. Esto es una simplificación bastante grande, ya que en realidad
#el lenguaje se crea por combinaciones no aleatorias de palabras, es decir, determinadas
#palabras tienden a utilizarse de forma conjunta. A continuación se muestran algunas formas de
#calcular, identificar y visualizar relaciones entre palabras

#La función unnest_tokens() del paquete tidytext permite dividir el texto por ngramas, 
#siendo cada n-grama una secuencia de n palabras consecutivas. 
#Para conseguir los ngramas, tiene que ser la función unnest_tokens() la que divida 
#el texto, por lo que se elimina la tokenización de la función limpiar_tokenizar

install.packages("tidytext")
library(tidytext)

limpiar <- function(texto){
  # El orden de la limpieza no es arbitrario
  # Se convierte todo el texto a minúsculas
    nuevo_texto <- tolower(texto)
    
  # Eliminación de páginas web (palabras que empiezan por "http." seguidas
  # de cualquier cosa que no sea un espacio)
    nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
    
  # Eliminación de signos de puntuación
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  
  # Eliminación de números
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  
  # Eliminación de espacios en blanco múltiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  return(nuevo_texto)
}

#Creación de bigramas de palabras

bigramas <- tweets %>% mutate(texto = limpiar(texto)) %>%
  select(texto) %>%
  unnest_tokens(input = texto, output = "bigrama",
                token = "ngrams",n = 2, drop = TRUE)

# Contaje de ocurrencias de cada bigrama
bigramas %>% count(bigrama, sort = TRUE)


#Los bigramas más frecuentes son los formados por stopwords. Como la relación entre estas
#palabras no aporta información de interés, se procede a eliminar todos aquellos bigramas que
#contienen alguna stopword.


bigrams_separados <- bigramas %>% separate(bigrama, c("palabra1", "palabra2"),sep = " ")
head(bigrams_separados)


-----------
# Filtrado de los bigramas que contienen alguna stopword


bigrams_separados <- bigrams_separados %>%
  filter(!palabra1 %in% lista_stopwords) %>%
  filter(!palabra2 %in% lista_stopwords)

# Unión de las palabras para formar de nuevo los bigramas
bigramas <- bigrams_separados %>%
  unite(bigrama, palabra1, palabra2, sep = " ")

# Nuevo contaje para identificar los bigramas más frecuentes
bigramas %>% count(bigrama, sort = TRUE) %>% print(n = 20)


#Una forma más visual e informativa de analizar las relaciones entre palabras es
#mediante el uso de networks. El paquete igraph permite crear networks a partir de
#dataframes que tenga una estructura de columnas de tipo: elemento_A, elemento_B,
#conexión.


install.packages("igraph")
install.packages("ggraph")

library(igraph)
library(ggraph)

graph <- bigramas %>%
  separate(bigrama, c("palabra1", "palabra2"), sep = " ") %>%
  count(palabra1, palabra2, sort = TRUE) %>%
  filter(n > 18) %>% graph_from_data_frame(directed = FALSE)
set.seed(123)

plot(graph, vertex.label.font = 2,
     vertex.label.color = "black",
     vertex.label.cex = 0.7, edge.color = "gray85")




#Con el paquete ggraph se pueden generar representaciones gráficas de networks basadas en
#ggplot2.


ggraph(graph = graph) +
  geom_edge_link(colour = "gray70") +
  geom_node_text(aes(label = name), size = 4) +
  theme_bw()


