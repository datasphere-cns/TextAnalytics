install.packages("syuzhet")

library(syuzhet)
library(RColorBrewer)
library(wordcloud)
library(tm)

#Mac y Linux
texto_cadena <- get_text_as_string("https://raw.githubusercontent.com/programminghistorian/jekyll/gh-pages/assets/galdos_miau.txt")

#Windows
texto_cadena <- scan(file = "https://raw.githubusercontent.com/programminghistorian/jekyll/gh-pages/assets/galdos_miau.txt", fileEncoding = "UTF-8", what = character(), sep = "\n", allowEscapes = T)


texto_cadena
texto_palabras <- get_tokens(texto_cadena)
head(texto_palabras)

length(texto_palabras)

oraciones_vector <- get_sentences(texto_cadena)
length(oraciones_vector)

sentimientos_df <- get_nrc_sentiment(texto_palabras, lang="spanish")

get_nrc_sentiment("me siento mal", lang="spanish")

get_nrc_sentiment("mal", lang="spanish")

get_nrc_sentiment("mal servicio", lang="spanish")
get_nrc_sentiment("estoy molesto", lang="spanish")

nrc_data <-get_nrc_sentiment("mal servicio AFP", lang="spanish")
nrc_data

nrc_data <-get_nrc_sentiment("ese servicio no sirve", lang="spanish")
nrc_data

get_nrc_sentiment("mal servicio", lang="spanish")


(nrc_data[, 9]*-1) + nrc_data[, 10]

valence <- (nrc_data[, 9]*-1) + nrc_data[, 10]
valence
