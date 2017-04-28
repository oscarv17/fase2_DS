#Crgando las librerias
install.packages("twitteR")
install.packages("ROAuth")
install.packages("base64enc")
install.packages("streamR")

#Cargas las librerias
library("ROAuth")
library("base64enc")
library("twitteR")
library("streamR")

# Cargar par?metros de configuraci?n
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
options(httr_oauth_cache=T)

# Cargar las credenciales obtenidas del paso anterior
consumer_key <- "RWiyhJrHzWHpIE6G3yiCEV6MQ"
consumer_secret <-"HXpU4rNxyKXvzQQfpvkdJ5xlwVd2hb8L95Gn5wS7cQ37DfLNyC"
access_token <-"139230277-Y7kRwzvl8n09b71wRdN77gIdcMSfJOy32jV3g0IL"
access_secret <-"AzCFSB80WLu9WkZgYiTW80vZOcR1NIpkyKgfQFh9r5KoC"

# Ejecutar la autenticaci?n de TwitteR
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# streamR authentication
credentials_file <- "my_oauth.Rdata"
if (file.exists(credentials_file)){
  load(credentials_file)
} else {
  cred <- OAuthFactory$new(consumerKey = consumer_key, consumerSecret =
                             consumer_secret, requestURL = reqURL, accessURL = accessURL, authURL = authURL)
  cred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  save(cred, file = credentials_file)
}
source('credenciales.R')

#Buscar tweets a traves del API
tweets <- searchTwitter("oscars OR #oscars OR #oscars2017 'mejor pelicula' OR #mejorpeli OR #mejorpelicula OR #mejorpelicula2017 OR #mifavoritaoscars gane OR quiero OR prefiero", n=1500, lang="es", since = "2017-02-25")
#make data frame
df <- do.call("rbind", lapply(tweets, as.data.frame))
#write to csv file (or your RODBC code)
write.csv(df,file="twitterList2.csv")
tweet <- tweets[[1]];

#Otra manera de pasar a un dataframe
tweet.df <- twListToDF(tweets)

# Mostrar la estructura del tweet
str(tweet)

# Obtener el texto del tweet:
tweet$getText()

#Limpieza 
unique(df)
df[!duplicated(df), ]
# En orden inverso
unique(df, fromLast = TRUE)
df[!duplicated(df, fromLast = TRUE), ]


