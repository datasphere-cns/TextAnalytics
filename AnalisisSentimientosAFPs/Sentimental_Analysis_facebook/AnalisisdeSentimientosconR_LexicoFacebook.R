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


#-------------------------------------------------------------------------
##---- Cargamos los datos especificando los tipos de datos de cada columna 

##-AFP Conf?a
facebook_AFPConfia<- read_csv( "AFPConfiaFacebook.csv", 
                               col_names = TRUE, col_types = cols(x1 = col_integer(), 
                                                                  x2 = col_character(), 
                                                                  Name = col_character(),
                                                                  ProfileID = col_character(),
                                                                  Date = col_character(),
                                                                  Likes  = col_integer(),
                                                                  Stars    = col_character(),
                                                                  Comment    = col_character(),
                                                                  view   = col_character()))

head(facebook_AFPConfia)
dim(facebook_AFPConfia)
str(facebook_AFPConfia)
colnames(facebook_AFPConfia)

##- Para el campo "Date" es necesario hacer una transformaci?n para adecuar el formato 
facebook_AFPConfia$Date <- as.Date(facebook_AFPConfia$Date,format="%d/%m/%Y")
facebook_AFPConfia$Date<- as.character(facebook_AFPConfia$Date)

## AFP Crecer
facebook_AFPCrecer<- read_csv( "AFPCrecerFacebook.csv", 
                               col_names = TRUE, col_types = cols(x1 = col_integer(), 
                                                                  x2 = col_character(), 
                                                                  Name = col_character(),
                                                                  ProfileID = col_character(),
                                                                  Date = col_character(),
                                                                  Likes  = col_integer(),
                                                                  Stars    = col_character(),
                                                                  Comment    = col_character(),
                                                                  view   = col_character()))



head(facebook_AFPCrecer)
dim(facebook_AFPCrecer)
str(facebook_AFPCrecer)
colnames(facebook_AFPCrecer)

##- Para el campo "Date" es necesario hacer una transformaci?n para adecuar el formato 
facebook_AFPCrecer$Date <- as.Date(facebook_AFPCrecer$Date,format="%d/%m/%Y")
facebook_AFPCrecer$Date<- as.character(facebook_AFPCrecer$Date)




#-------------------------------------------------------------------------
##--Verificaci?n que los campos de todos los DF tengan el mismo nombre
colnames(facebook_AFPConfia)
colnames(facebook_AFPCrecer)

#-------------------------------------------------------------------------

#Agregamos una columnna que servir? para identificar los tweets de las diferentes empresas 
facebook_AFPCrecer <- cbind(facebook_AFPCrecer, account= c("AFPCrecer"))
facebook_AFPConfia <- cbind(facebook_AFPConfia, account= c("AFPConfia"))


#-------------------------------------------------------------------------
# Unimos los DF  en un unico dataframe
comentsbind <- bind_rows(facebook_AFPConfia,facebook_AFPCrecer)


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
.


#-------------------------------------------------------------------------
#Excluir cuentas ra?z del community manager de cada empresa
comentsbind <- comentsbind[ !(comentsbind$autor %in% c('AFP Crecer','AFP CONFIA' )), ]



## Descargamos este léxico si aun no lo hemos descargado
#download.file("https://raw.githubusercontent.com/jboscomendoza/rpubs/master/sentimientos_afinn/lexico_afinn.en.es.csv","lexico_afinn.en.es.csv")

afinn <- read.csv("lexico_afinn.en.es.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% tbl_df()


head(comentsbind)

## Preparando los datos
## Manipulamos la columna fecha con la función separate() de tidyr.
comentsbind <- comentsbind %>% separate(fecha, into = c("FechaNew", "Hora"), sep = " ")


## Convirtiendo tuits en palabras
## Usamos la función unnest_token() de tidytext, que tomara los tuits en la 
## columna text y los separa en una nueva columna llamada "Palabra". Hecho esto, 
## usamos left_join() de dplyr, para unir los objetos,  a partir del
## contenido de la columna "Palabra". De este modo, obtendremos un data frame que 
## contiene sólo los tuits con palabras presentes en el léxico Afinn.

## Además, aprovechamos para crear una columna con mutate() de dplyr a las 
## palabras como Positiva o Negativa. Llamaremos esta columna Tipo y cambiamos el


commets_afinn <- 
  comentsbind %>%
  unnest_tokens(input = "texto", output = "Palabra") %>%
  inner_join(afinn, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa"))
  

## Obtenemos también una puntuación por tuit, usando group_by() y summarise() de 
## dplyr, y le agregamos tuits para usarla después. Tambien asignamos a los tuits
## sin puntuación positiva o negativa un valor de 0, que indica neutralidad. 




## Con esto estamos listos para empezar.
## Empecemos revisando cuántas palabras en total y cuantas palabras únicas  
## con count(), group_by() y distinct() de dplyr.

# Total
commets_afinn %>%
  count(empresa)

# Únicas
commets_afinn %>% 
  group_by(empresa) %>% 
  distinct(Palabra) %>% 
  count()

## Y veamos también las palabras positivas y negativas más usadas 
## de ellos, usando map() de purr, top_n() de dplyr() y ggplot.

map(c("Positiva", "Negativa"), function(sentimiento) {
  commets_afinn %>%
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

commets_afinn <-
  commets_afinn %>%
  filter(Palabra != "no") 

## Como deseamos observar tendencias, vamos a obtener la media de sentimientos 
## por día, usando group_by() y summarise() y asignamos los resultados 
## a tuits_afinn_fecha



commets_afinn_fecha <-
  commets_afinn %>%
  group_by(empresa) %>%
  mutate(Suma = mean(Puntuacion)) %>%
  group_by(empresa, FechaNew) %>%
  summarise(Media = mean(Puntuacion))

## Veamos nuestros resultados con ggplot()

commets_afinn_fecha %>%
  ggplot() +
  aes( as.Date(FechaNew), Media, color = empresa) +
  geom_line() +
  tema_graf +
  theme(legend.position = "top")



## Si separamos las líneas, usando facet_wrap(), será más fácil observar el las tendencias

commets_afinn_fecha %>%
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
## para calcular y graficar una regresión a partir de las medias por día.

commets_afinn_fecha %>%
  ggplot() +
  aes(as.Date(FechaNew), Media, color = empresa) +
  geom_smooth(method = "loess", fill = NA) +
  tema_graf

## En realidad, podemos obtener líneas muy similares directamente de las puntuaciones.



commets_afinn %>%
  ggplot() +
  aes(as.Date(FechaNew), Puntuacion, color = empresa) +
  geom_smooth(method = "loess", fill = NA) +
  tema_graf

## Si separamos las lineas y mostramos los puntos a partir de
## los cuales se obtienen las líneas de regresión, podemos observar con más 
## claridad la manera en que el algoritmo LOESS llega a sus resultado. 
## Haremos esto con facet_wrap() y geom_point

commets_afinn %>%
  ggplot() +
  aes(as.Date(FechaNew), Puntuacion, color = empresa) +
  geom_point(color = "#E5E5E5") + 
  geom_smooth(method = "loess", fill = NA) +
  facet_wrap(~empresa) +
  tema_graf

## Comparemos los resultados de al algoritmo LOESS con los resultados de una
## Regresión Lineal ordinaria, que intentará ajustar una recta.

commets_afinn_fecha %>%
  ggplot() +
  aes(as.Date(FechaNew), Media, color = empresa) +
  geom_point(color = "#E5E5E5") + 
  geom_smooth(method = "lm", fill = NA) +
  facet_wrap(~empresa) +
  tema_graf

## Usando la media móvil
## Crearemos medias móviles usando rollmean() de zoo. 
##Con esta función calculamos la media de cada tres días y la graficamos con ggplot.

commets_afinn_fecha %>%
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

commets_afinn %>%
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

commets_afinn %>%
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
## Creamos un boxplot por candidato.

commets_afinn %>%
  ggplot() +
  aes(empresa, Puntuacion, fill = empresa) +
  geom_boxplot() +
  tema_graf



## Usando densidades
## Por último, podemos analizar las tendencias de sentimientos usando las 
## funciones de densidad de las puntuaciones. ggplot2 tiene la función 
## geom_density() que hace muy fácil crear y graficar estas funciones.

commets_afinn %>%
  ggplot() +
  aes(Puntuacion, color = empresa) +
  geom_density() +
  facet_wrap(~empresa) +
  tema_graf





