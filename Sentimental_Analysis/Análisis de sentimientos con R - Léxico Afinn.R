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

## Descargamos el archivo con los tuits si aun no los hemos descargado
download.file("https://raw.githubusercontent.com/jboscomendoza/rpubs/master/sentimientos_afinn/tuits_candidatos.csv", "tuits_candidatos.csv")

## Importamos los tuits usando read.csv(). 
## El argumento fileEncoding = "latin1" es importante para mostrar correctamente 
## las vocales con tildes, la ñ u otros caracteres especiales.

tuits <- read.csv("tuits_candidatos.csv", stringsAsFactors = F,  fileEncoding = "latin1") %>%  tbl_df()

## Descargamos este léxico si aun no lo hemos descargado
download.file("https://raw.githubusercontent.com/jboscomendoza/rpubs/master/sentimientos_afinn/lexico_afinn.en.es.csv","lexico_afinn.en.es.csv")

## De nuevo usamos la función read.csv() para importar los datos.

afinn <- read.csv("lexico_afinn.en.es.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% tbl_df()

str(tuits)
str(afinn)

## Preparando los datos
## Manipulamos la columna created_at con la función separate() de tidyr.
## Separamos esta columna en una fecha y hora del día, y después separaremos la 
## fecha en día, mes y año.
## Por último, usamos filter() de dplyr para seleccionar sólo los tuits hechos en
## el 2018.

tuits <- 
  tuits %>%
  separate(created_at, into = c("Fecha", "Hora"), sep = " ") %>%
  separate(Fecha, into = c("Dia", "Mes", "Periodo"), sep = "/",
           remove = FALSE) %>%
  mutate(Fecha = dmy(Fecha),
         Semana = week(Fecha) %>% as.factor(),
         text = tolower(text)) %>%
  filter(Periodo == 2018)

str(tuits)

## Convirtiendo tuits en palabras
## Usamos la función unnest_token() de tidytext, que tomara los tuits en la 
## columna text y los separá en una nueva columna llamada "Palabra". Hecho esto, 
## usamos left_join() de dplyr, para unir los objetos tuits y afinn, a partir del
## contenido de la columna "Palabra". De este modo, obtendremos un data frame que 
## contiene sólo los tuits con palabras presentes en el léxico Afinn.

## Además, aprovechamos para crear una columna con mutate() de dplyr a las 
## palabras como Positiva o Negativa. Llamaremos esta columna Tipo y cambiamos el
## nombre de la columna screen_name a Candidato.

tuits_afinn <- 
  tuits %>%
  unnest_tokens(input = "text", output = "Palabra") %>%
  inner_join(afinn, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa")) %>% 
  rename("Candidato" = screen_name)


str(afinn)
str(tuits_afinn)
## Obtenemos también una puntuación por tuit, usando group_by() y summarise() de 
## dplyr, y le agregamos tuits para usarla después. Tambien asignamos a los tuits
## sin puntuación positiva o negativa un valor de 0, que indica neutralidad. 
## Por último cambiamos el nombre de la columna screen_name a Candidato.



## Con esto estamos listos para empezar.
## Explorando los datos, medias por día
## Empecemos revisando cuántas palabras en total y cuantas palabras únicas ha 
## usado cada candidato con count(), group_by() y distinct() de dplyr.

# Total
tuits_afinn %>%
  count(Candidato)

# Únicas
tuits_afinn %>% 
  group_by(Candidato) %>% 
  distinct(Palabra) %>% 
  count()

## Y veamos también las palabras positivas y negativas más usadas por cada uno 
## de ellos, usando map() de purr, top_n() de dplyr() y ggplot.

map(c("Positiva", "Negativa"), function(sentimiento) {
  tuits_afinn %>%
    filter(Tipo ==  sentimiento) %>%
    group_by(Candidato) %>%
    count(Palabra, sort = T) %>%
    top_n(n = 10, wt = n) %>%
    ggplot() +
    aes(Palabra, n, fill = Candidato) +
    geom_col() +
    facet_wrap("Candidato", scales = "free") +
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

## Como deseamos observar tendencias, vamos a obtener la media de sentimientos 
## por día, usando group_by() y summarise() y asignamos los resultados 
## a tuits_afinn_fecha

tuits_afinn_fecha <-
  tuits_afinn %>%
  group_by(status_id) %>%
  mutate(Suma = mean(Puntuacion)) %>%
  group_by(Candidato, Fecha) %>%
  summarise(Media = mean(Puntuacion))

## Veamos nuestros resultados con ggplot()

tuits_afinn_fecha %>%
  ggplot() +
  aes(Fecha, Media, color = Candidato) +
  geom_line() +
  tema_graf +
  theme(legend.position = "top")

## Si separamos las líneas por candidato, usando facet_wrap(), será más fácil 
## observar el las tendencias de los Candidatos.

tuits_afinn_fecha %>%
  ggplot() +
  aes(Fecha, Media, color = Candidato) +
  geom_hline(yintercept = 0, alpha = .35) +
  geom_line() +
  facet_grid(Candidato~.) +
  tema_graf +
  theme(legend.position = "none")


## Una explicación más completa de LOESS se encuentra aquí:
## https://www.itl.nist.gov/div898/handbook/pmd/section1/pmd144.htm

## Usamos la función geom_smooth() de ggplot2, con el argumento method = "loess"
## para calcular y graficar una regresión local a partir de las medias por día.

tuits_afinn_fecha %>%
  ggplot() +
  aes(Fecha, Media, color = Candidato) +
  geom_smooth(method = "loess", fill = NA) +
  tema_graf

## En realidad, podemos obtener líneas muy similares directamente de 
## las puntuaciones.

tuits_afinn %>%
  ggplot() +
  aes(Fecha, Puntuacion, color = Candidato) +
  geom_smooth(method = "loess", fill = NA) +
  tema_graf

## Si separamos las lineas por candidato y mostramos los puntos a partir de
## los cuales se obtienen las líneas de regresión, podemos observar con más 
## claridad la manera en que el algoritmo LOESS llega a sus resultado. 
## Haremos esto con facet_wrap() y geom_point

tuits_afinn %>%
  ggplot() +
  aes(Fecha, Puntuacion, color = Candidato) +
  geom_point(color = "#E5E5E5") + 
  geom_smooth(method = "loess", fill = NA) +
  facet_wrap(~Candidato) +
  tema_graf

## Comparemos los resultados de al algoritmo LOESS con los resultados de una
## Regresión Lineal ordinaria, que intentará ajustar una recta.

tuits_afinn_fecha %>%
  ggplot() +
  aes(Fecha, Media, color = Candidato) +
  geom_point(color = "#E5E5E5") + 
  geom_smooth(method = "lm", fill = NA) +
  facet_wrap(~Candidato) +
  tema_graf

## Usando la media móvil
## Crearemos medias móviles usando rollmean() de zoo. Con esta función calculamos
## la media de cada tres días y la graficamos con ggplot.

tuits_afinn_fecha %>%
  group_by(Candidato) %>%
  mutate(MediaR = rollmean(Media, k = 3, align = "right", na.pad = TRUE)) %>%
  ggplot() +
  aes(Fecha, MediaR, color = Candidato) +
  geom_hline(yintercept = 0, alpha = .35) +
  geom_line() +
  facet_grid(Candidato~.) +
  tema_graf


## Comparando sentimientos positivos y negativos
## veamos que proporción de tuits fueron positivos y negativos, para todo el 2018
## y para cada Candidato. Usamos geom_col() de ggplot2 para elegir el tipo de 
## gráfica y la función percent_format() de scales para dar formato de porcentaje
## al eje y.

tuits_afinn %>%
  count(Candidato, Tipo) %>%
  group_by(Candidato) %>%
  mutate(Proporcion = n / sum(n)) %>%
  ggplot() +
  aes(Candidato, Proporcion, fill = Tipo) +
  geom_col() +
  scale_y_continuous(labels = percent_format()) +
  tema_graf +
  theme(legend.position = "top")

## Usamos el argumento width = 1 de geom_col() para quitar el espacio entre barras
## individuales y el argumento expand = c(0, 0) de scale_x_date() para quitar el
## espacio en blanco en los extremos del eje x de nuestra gráfica

tuits_afinn %>%
  group_by(Candidato, Fecha) %>%
  count(Tipo) %>%
  mutate(Proporcion = n / sum(n)) %>%
  ggplot() +
  aes(Fecha, Proporcion, fill = Tipo) +
  geom_col(width = 1) +
  facet_grid(Candidato~.) +
  scale_y_continuous(labels = percent_format()) +
  scale_x_date(expand = c(0, 0)) +
  tema_graf +
  theme(legend.position = "top")

## Bloxplots (diagrama caja y bigotes)
## Usamos la función geom_boxplot() de ggplot2 para elegir el tipo de gráfica. 
## Creamos un boxplot por candidato.

tuits_afinn %>%
  ggplot() +
  aes(Candidato, Puntuacion, fill = Candidato) +
  geom_boxplot() +
  tema_graf

## usamos factor() dentro de mutate() para cambiar el tipo de dato de Mes, en R 
## los boxplots necesitan una variable discreta en el eje x para mostrarse 
## correctamente.

tuits_afinn %>%
  mutate(Mes = factor(Mes)) %>% 
  ggplot() +
  aes(Mes, Puntuacion, fill = Candidato) +
  geom_boxplot(width = 1) +
  facet_wrap(~Candidato) +
  tema_graf +
  theme(legend.position = "none")

## Usando densidades
## Por último, podemos analizar las tendencias de sentimientos usando las 
## funciones de densidad de las puntuaciones. ggplot2 tiene la función 
## geom_density() que hace muy fácil crear y graficar estas funciones.

tuits_afinn %>%
  ggplot() +
  aes(Puntuacion, color = Candidato) +
  geom_density() +
  facet_wrap(~Candidato) +
  tema_graf

## Por supuesto, también podemos observar las tendencias a través del tiempo 
## usando facet_grid() para crear una cuadrícula de gráficas, con los 
## candidatos en el eje x y los meses en el eje y.

tuits_afinn %>%
  ggplot() +
  aes(Puntuacion, color = Candidato) +
  geom_density() +
  facet_grid(Candidato~Mes) +
  tema_graf






