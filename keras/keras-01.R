install.packages('tensorflow')
install.packages('Rcpp')

library(tidyverse)

library('Rcpp')
library(tensorflow)
#install_tensorflow(version = "2.0.0")
install_tensorflow()
library(keras)
library(dplyr)
library(ggplot2)
library(purrr)


df <- read_csv("C:/Users/nelso/Desktop/Datasphere/TextAnalytics-main/keras/movie_review.csv")

head(df)
str(df)
dim(df)
df %>% count(tag)

df$text[5]
head(df)

df_small_id <- sample.int(nrow(df), size = nrow(df)*1)

df_small<-df[df_small_id,]
str(df_small)
df_small %>% count(tag)


training_id <- sample.int(nrow(df_small), size = nrow(df_small)*0.8)
training <- df[training_id,]
testing <- df[-training_id,]

df_small$text %>% 
  strsplit(" ") %>% 
  sapply(length) %>% 
  summary()

num_words <- 10000
max_length <- 50
text_vectorization <- layer_text_vectorization(
  max_tokens = num_words, 
  output_sequence_length = max_length, 
)


text_vectorization %>% 
  adapt(df_small$text)

get_vocabulary(text_vectorization)

text_vectorization(matrix(df_small$text[1], ncol = 1))



input <- layer_input(shape = c(1), dtype = "string")

output <- input %>% 
  text_vectorization() %>% 
  layer_embedding(input_dim = num_words + 1, output_dim = 16) %>%
  layer_global_average_pooling_1d() %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dropout(0.5) %>% 
  layer_dense(units = 1, activation = "sigmoid")

model <- keras_model(input, output)


model %>% compile(
  optimizer = 'adam',
  loss = 'binary_crossentropy',
  metrics = list('accuracy')
)


history <- model %>% fit(
  training$text,
  as.numeric(training$tag == "pos"),
  epochs = 40,
  batch_size = 512,
  validation_split = 0.2,
  verbose=2
)

head(testing$text)

results <- model %>% evaluate(testing$text, as.numeric(testing$tag == "pos"), verbose = 0)
results

plot(history)

model %>% predict("for starters , it was created by alan moore ( and eddie campbell ) , who brought the medium to a whole new level in the mid '80s")

model %>% predict("in other words , don't dismiss this film because of its source")
