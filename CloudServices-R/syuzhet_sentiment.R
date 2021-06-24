install.packages("syuzhet")

library(syuzhet)
library(RColorBrewer)
library(wordcloud)
library(tm)


get_nrc_sentiment("me siento mal", lang="spanish")

get_nrc_sentiment("mal", lang="spanish")

get_nrc_sentiment("mal servicio", lang="spanish")
get_nrc_sentiment("estoy molesto", lang="spanish")

nrc_data <-get_nrc_sentiment("mal servicio AFP", lang="spanish")
nrc_data



(nrc_data[, 9]*-1) + nrc_data[, 10]

valence <- (nrc_data[, 9]*-1) + nrc_data[, 10]
valence



get_nrc_sentiment("excelente servicio", lang="spanish")

