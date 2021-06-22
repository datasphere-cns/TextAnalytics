## Cargar librerias
install.packages("zoo")


library(tidyverse)
library(tidytext)
library(tm)
library(lubridate)
library(zoo)
library(scales)

## Definimos un tema para facilitar la visualización

tema_graf <-
  theme_minimal() +
  theme(text = element_text(family = "serif"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#EBEBEB", colour = NA),
        legend.position = "none",
        legend.box.background = element_rect(fill = "#EBEBEB", colour = NA))

##Cargamos los datasets
tweets_AFPConfia<- read_csv( "UTF8_AFPConfia.csv", 
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


str(tweets_AFPConfia)
head(tweets_AFPConfia)


tweets_AFPCrecer<- read_csv( "UTF8_AFPCrecer.csv", 
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

##- Para el campo "Date" es necesario hacer una transformaci?n para adecuar el formato 
tweets_AFPCrecer$Date <- as.Date(tweets_AFPCrecer$Date,format="%d/%m/%Y")
tweets_AFPCrecer$Date<- as.character(tweets_AFPCrecer$Date)

str(tweets_AFPCrecer)
head(tweets_AFPCrecer)

#-------------------------------------------------------------------------

#Agregamos una columnna que servir? para identificar los tweets de las diferentes empresas 
tweets_AFPCrecer <- cbind(tweets_AFPCrecer, account= c("AFPCrecer"))
tweets_AFPConfia <- cbind(tweets_AFPConfia, account= c("AFPConfia"))

#-------------------------------------------------------------------------
# Unimos los DF  en un unico dataframe
tweetsbind <- bind_rows(tweets_AFPConfia,tweets_AFPCrecer)


#-------------------------------------------------------------------------
# Selecci?n de variables
tweetsbind <- tweetsbind %>% select(Username, Date, TweetText, account, TweetID)


#-------------------------------------------------------------------------

# Se renombran algunas  variables con nombres m?s pr?cticos
tweetsbind <- tweetsbind %>% rename(autor = Username, fecha = Date, texto = TweetText, empresa= account)
 


#-------------------------------------------------------------------------
#Excluir cuentas ra?z del community manager de cada empresa
tweetsbind <- tweetsbind[ !(tweetsbind$autor %in% c('AFPCrecer','AFPCONFIA' )), ]

str(tweetsbind)


## Descargamos este léxico si aun no lo hemos descargado
#download.file("https://raw.githubusercontent.com/jboscomendoza/rpubs/master/sentimientos_afinn/lexico_afinn.en.es.csv","lexico_afinn.en.es.csv")

## De nuevo usamos la función read.csv() para importar los datos.

afinn <- read.csv("lexico_afinn.en.es.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% tbl_df()


## Preparando los datos
## Manipulamos la columna fecha con la función separate() de tidyr.
tweetsbind <- tweetsbind %>% separate(fecha, into = c("FechaNew", "Hora"), sep = " ")



## Convirtiendo tuits en palabras
## Usamos la función unnest_token() de tidytext, que tomara la 
## columna text y los separá en una nueva columna llamada "Palabra". Hecho esto, 
## usamos left_join() de dplyr, para unir los objetos, a partir del
## contenido de la columna "Palabra". De este modo, obtendremos un data frame que 
## contiene sólo los tuits con palabras presentes en el léxico Afinn.

## Además, aprovechamos para crear una columna con mutate() de dplyr a las 
## palabras como Positiva o Negativa. Llamaremos esta columna Tipo y cambiamos el


tuits_afinn <- 
  tweetsbind %>%
  unnest_tokens(input = "texto", output = "Palabra") %>%
  inner_join(afinn, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa"))
  

## Obtenemos también una puntuación por tuit, usando group_by() y summarise() de 
## dplyr, y le agregamos tuits para usarla después. Tambien asignamos a los tuits
## sin puntuación positiva o negativa un valor de 0, que indica neutralidad. 



## Con esto estamos listos para empezar.
## Explorando los datos, medias por día
## Empecemos revisando cuántas palabras en total y cuantas palabras únicas ha 
## usado cada candidato con count(), group_by() y distinct() de dplyr.

# Total
tuits_afinn %>%
  count(empresa)

# Únicas
tuits_afinn %>% 
  group_by(empresa) %>% 
  distinct(Palabra) %>% 
  count()

## Y veamos también las palabras positivas y negativas más usadas por cada uno 
## de ellos, usando map() de purr, top_n() de dplyr() y ggplot.

map(c("Positiva", "Negativa"), function(sentimiento) {
  tuits_afinn %>%
    filter(Tipo ==  sentimiento) %>%
    group_by(empresa) %>%
    count(Palabra, sort = T) %>%
    top_n(n = 10, wt = n) %>%
    ggplot() +
    aes(Palabra, n, fill = empresa) +
    geom_col() +
    facet_wrap("empresa", scales = "free") +
    scale_y_continuous(expand = c(0, 0)) +
    coord_flip() +
    labs(title = sentimiento) +
    tema_graf
})

## Quitamos “no” de nuestras palabras. Es una palabra muy comun en español que
## no necesariamente implica un sentimiento negativo.

tuits_afinn <-
  tuits_afinn %>%
  filter(Palabra != "no") 

tuits_afinn %>%
  filter(empresa == "AFPCrecer") 

## Como deseamos observar tendencias, vamos a obtener la media de sentimientos 
## por día, usando group_by() y summarise() y asignamos los resultados 
## a tuits_afinn_fecha


tuits_afinn_fecha <-
  tuits_afinn %>%
  group_by(TweetID) %>%
  mutate(Suma = mean(Puntuacion)) %>%
  group_by(empresa, FechaNew) %>%
  summarise(Media = mean(Puntuacion))

## Veamos nuestros resultados con ggplot()

tuits_afinn_fecha %>%
  ggplot() +
  aes( as.Date(FechaNew), Media, color = empresa) +
  geom_line() +
  tema_graf +
  theme(legend.position = "top")



## Si separamos las líneas por candidato, usando facet_wrap(), será más fácil observar el las tendencias

tuits_afinn_fecha %>%
  ggplot() +
  aes(as.Date(FechaNew), Media, color = empresa) +
  geom_hline(yintercept = 0, alpha = .35) +
  geom_line() +
  facet_grid(empresa~.) +
  tema_graf +
  theme(legend.position = "none")


## Una explicación más completa de LOESS se encuentra aquí:
## https://www.itl.nist.gov/div898/handbook/pmd/section1/pmd144.htm

## Usamos la función geom_smooth() de ggplot2, con el argumento method = "loess"
## para calcular y graficar una regresión local a partir de las medias por día.

tuits_afinn_fecha %>%
  ggplot() +
  aes(as.Date(FechaNew), Media, color = empresa) +
  geom_smooth(method = "loess", fill = NA) +
  tema_graf

## En realidad, podemos obtener líneas muy similares directamente de las puntuaciones.

tuits_afinn %>%
  ggplot() +
  aes(as.Date(FechaNew), Puntuacion, color = empresa) +
  geom_smooth(method = "loess", fill = NA) +
  tema_graf

## Si separamos las lineas por candidato y mostramos los puntos a partir de
## los cuales se obtienen las líneas de regresión, podemos observar con más 
## claridad la manera en que el algoritmo LOESS llega a sus resultado. 
## Haremos esto con facet_wrap() y geom_point

tuits_afinn %>%
  ggplot() +
  aes(as.Date(FechaNew), Puntuacion, color = empresa) +
  geom_point(color = "#E5E5E5") + 
  geom_smooth(method = "loess", fill = NA) +
  facet_wrap(~empresa) +
  tema_graf

## Comparemos los resultados de al algoritmo LOESS con los resultados de una
## Regresión Lineal ordinaria, que intentará ajustar una recta.

tuits_afinn_fecha %>%
  ggplot() +
  aes(as.Date(FechaNew), Media, color = empresa) +
  geom_point(color = "#E5E5E5") + 
  geom_smooth(method = "lm", fill = NA) +
  facet_wrap(~empresa) +
  tema_graf

## Usando la media móvil
## Crearemos medias móviles usando rollmean() de zoo. Con esta función calculamos
## la media de cada tres días y la graficamos con ggplot.

tuits_afinn_fecha %>%
  group_by(empresa) %>%
  mutate(MediaR = rollmean(Media, k = 3, align = "right", na.pad = TRUE)) %>%
  ggplot() +
  aes(as.Date(FechaNew), MediaR, color = empresa) +
  geom_hline(yintercept = 0, alpha = .35) +
  geom_line() +
  facet_grid(empresa~.) +
  tema_graf


## Comparando sentimientos positivos y negativos
#Usamos geom_col() de ggplot2 para elegir el tipo de 
## gráfica y la función percent_format() de scales para dar formato de porcentaje
## al eje y.

tuits_afinn %>%
  count(empresa, Tipo) %>%
  group_by(empresa) %>%
  mutate(Proporcion = n / sum(n)) %>%
  ggplot() +
  aes(empresa, Proporcion, fill = Tipo) +
  geom_col() +
  scale_y_continuous(labels = percent_format()) +
  tema_graf +
  theme(legend.position = "top")

## Usamos el argumento width = 1 de geom_col() para quitar el espacio entre barras
## individuales y el argumento expand = c(0, 0) de scale_x_date() para quitar el
## espacio en blanco en los extremos del eje x de nuestra gráfica

tuits_afinn %>%
  group_by(empresa, FechaNew) %>%
  count(Tipo) %>%
  mutate(Proporcion = n / sum(n)) %>%
  ggplot() +
  aes(as.Date(FechaNew), Proporcion, fill = Tipo) +
  geom_col(width = 1) +
  facet_grid(empresa~.) +
  scale_y_continuous(labels = percent_format()) +
  scale_x_date(expand = c(0, 0)) +
  tema_graf +
  theme(legend.position = "top")

## Bloxplots (diagrama caja y bigotes)
## Usamos la función geom_boxplot() de ggplot2 para elegir el tipo de gráfica. 


tuits_afinn %>%
  ggplot() +
  aes(empresa, Puntuacion, fill = empresa) +
  geom_boxplot() +
  tema_graf



## Usando densidades
## Por último, podemos analizar las tendencias de sentimientos usando las 
## funciones de densidad de las puntuaciones. ggplot2 tiene la función 
## geom_density() que hace muy fácil crear y graficar estas funciones.

tuits_afinn %>%
  ggplot() +
  aes(Puntuacion, color = empresa) +
  geom_density() +
  facet_wrap(~empresa) +
  tema_graf





