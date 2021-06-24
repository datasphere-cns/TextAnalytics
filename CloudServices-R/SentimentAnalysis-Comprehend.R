install.packages("rtweet")
library("rtweet")
  appname <- "TextMiningNZ"
  key <- "8tDD9lLuBucgskOo0SYN5HM2D"
  secret_key <- "1dfuTqnWhEfHbUx6gpEEAtCnKTmxOKTC2hOVvnnEyj6nvq1gyc"
  access_token <- "550690361-okqhRgDEO4PZN7IUVawAlqz5NYwA0LUNOCI39elQ"
  access_secret <- "X3TR0cdDlCQr1zOJTsewoVVUmy0cEyXeXnENvAoGVmzGt"
  # Create token named "twitter_token"
  twitter_token <- create_token(
    app = appname,
    consumer_key = key,
    consumer_secret = secret_key,
    access_token = access_token,
    access_secret = access_secret)

  install.packages("aws.comprehend")
  # Install package if needed: install.packages("aws.comprehend")
  AWS_ACCESS_KEY_ID <- "AKIAZPLL4MKEQHIX2MOX"
  AWS_SECRET_ACCESS_KEY <- "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"
  #A ctivate keys
  Sys.setenv("AWS_ACCESS_KEY_ID" = AWS_ACCESS_KEY_ID,
             "AWS_SECRET_ACCESS_KEY" = AWS_SECRET_ACCESS_KEY,
             "AWS_DEFAULT_REGION" = "eu-west-1") 
  library(aws.comprehend)
  detect_sentiment("Hey, I'm feeling great today!")# Try out Amazon comprehend!
  detect_sentiment("Comida horrible")# Try out Amazon comprehend!
  detect_sentiment("La atencion al cliente es pesima")
  detect_sentiment("Elvis se cambiara de trabajo")
  detect_sentiment("Las pupusas son buenas")
  
  
  # Get twitter data
  trainer <- search_tweets(q = "afpconfia", n=100, type='mixed', lang="es") 
  # Source for URL removal: https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/text-mining-twitter-data-intro-r/
  trainer$stripped_text <- gsub("http.*","",  trainer$text) # Remove http
  trainer$stripped_text <- gsub("https.*","", trainer$stripped_text) # Remove https
  # Emoji removal
  trainer$plain_tweet <- enc2native(trainer$stripped_text) # Covnert emojis to native encoding
  trainer$plain_tweet <- gsub("<.*.>", "", trainer$plain_tweet)
  trainer$plain_tweet <- trimws(trainer$plain_tweet) # Remove leading whitespaces from the beginning
  
  
  head(trainer)
  head(trainer$text)
  
  sentiment <- function(row, df) {
    record <- df[row,]
    sentiment <- detect_sentiment(as.character(record$text))   # Get sentiment from Amazon's Comprehemd
    merged <- merge(sentiment, record) # Merge the sentiment result to the original data
    return (merged)}
  
  row_seq <- seq(1,nrow(trainer)) # Define argument for lapply
  
  row_seq
  sentiment <- lapply(row_seq, sentiment, df=trainer)
  
head(sentiment, n=1)
install.packages("data.table")    
library("data.table")
appended_df4 <- rbindlist(sentiment) # Append list of dataframes together

df_sentiment<-appended_df4$Sentiment
df_text<-appended_df4$text

final_df<-cbind(data.frame(df_text), data.frame(df_sentiment))

str(final_df)
write.csv(final_df,"/Users/nelsonzepeda/Desktop/Datasphere/TextAnalytics-main/export-comprehend-confia.csv", row.names = FALSE)
