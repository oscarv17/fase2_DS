install.packages(c("tm","RColorBrewer", "wordcloud","topicmodels","LDAvis","dplyr","stringi"))

library(tm)
library(wordcloud)
library(topicmodels)
library(LDAvis)
library(dplyr)
library(stringi)

tweets <- read.csv("tweetsOscars.csv", header = TRUE, sep = ",")

textT <- tweets$text

tweetCorpus <- Corpus(VectorSource(textT))


tweetCorpus <- tm_map(tweetCorpus, content_transformer(tolower)) #convertir en minuscula

removeMention <- function(x) gsub("@\\w+","", x) #quitar las menciones
tweetCorpus <- tm_map(tweetCorpus, content_transformer(removeMention))

tweetCorpus <- tm_map(tweetCorpus, removePunctuation) #remover signos de puntuacion

tweetCorpus <- tm_map(tweetCorpus, removeNumbers) #remover las numeraciones

removeURL <- function(x) gsub("http[[:alnum:]]*","", x) #remover los enlaces
tweetCorpus <- tm_map(tweetCorpus, content_transformer(removeURL))

removeAccencts <- function(x) chartr("áéíóú", "aeiou", x) #remover los acentos
tweetCorpus <- tm_map(tweetCorpus, content_transformer(removeAccencts))

# Se concatenan los nombres de las peliculas papra que formen una sola palabra#

concatMovie1 <- function(x) gsub("la la land", "lalaland", x)
tweetCorpus <- tm_map(tweetCorpus, content_transformer(concatMovie1))

concatMovie2 <- function(x) gsub("hacksaw ridge", "hacksawridge", x)
tweetCorpus <- tm_map(tweetCorpus, content_transformer(concatMovie2))

concatMovie3 <- function(x) gsub("hell or high water", "hellorhighwater", x)
tweetCorpus <- tm_map(tweetCorpus, content_transformer(concatMovie3))

concatMovie4 <- function(x) gsub("hidden figures", "hiddenfigures", x)
tweetCorpus <- tm_map(tweetCorpus, content_transformer(concatMovie4))

concatMovie5 <- function(x) gsub("manchester by the sea", "manchesterbythesea", x)
tweetCorpus <- tm_map(tweetCorpus, content_transformer(concatMovie5))

# Se estandarizan los nombres de las peliculas al ingles #

translateMovie1 <- function(x) gsub("luz de luna", "moonlight", x)
tweetCorpus <- tm_map(tweetCorpus, content_transformer(translateMovie1))

translateMovie2 <- function(x) gsub("hasta el ultimo hombre", "hacksawridge", x)
tweetCorpus <- tm_map(tweetCorpus, content_transformer(translateMovie2))

translateMovie3 <- function(x) gsub("sin nada que perder", "hellorhighwater", x)
tweetCorpus <- tm_map(tweetCorpus, content_transformer(translateMovie3))

translateMovie4 <- function(x) gsub("figuras ocultas", "hiddenfigures", x)
tweetCorpus <- tm_map(tweetCorpus, content_transformer(translateMovie4))

translateMovie5 <- function(x) gsub("la llegada", "arrival", x)
tweetCorpus <- tm_map(tweetCorpus, content_transformer(translateMovie5))

translateMovie6 <- function(x) gsub("un camino a casa", "lion", x)
tweetCorpus <- tm_map(tweetCorpus, content_transformer(translateMovie6))

translateMovie7 <- function(x) gsub("manchester junto al mar", "manchesterbythesea", x)
tweetCorpus <- tm_map(tweetCorpus, content_transformer(translateMovie7))


tweetCorpus <- tm_map(tweetCorpus, removeWords, stopwords("spanish")) #remueve los stopwords del paquete

myStopWords <- readLines("stopwords.txt",encoding="UTF-8") #remueve los stopwords personales
myStopWords <- iconv(myStopWords, to="ASCII//TRANSLIT")
tweetCorpus <- tm_map(tweetCorpus, removeWords,myStopWords)

tweetCorpus <- tm_map(tweetCorpus, stripWhitespace) #quita los espacios en blanco

tdm <- TermDocumentMatrix(tweetCorpus)

toDataFrame <- function(tdm){
  #convierte a una matriz
  mat <- as.matrix(tdm)
  
  #conteo de palabras en orden decreciente
  count <- sort(rowSums(mat),decreasing=TRUE)
  
  #crea un data frame con las palabras y sus frecuencias
  df <- data.frame(word = names(count), freq=count)
  
  return(df)
}

dataF <- toDataFrame(tdm)

wordcloud(dataF$word, dataF$freq, min.freq = 5,random.order=FALSE, colors=brewer.pal(8, "Set2"))

#hay que eliminar las palabras que no generan informacion y son muy frecuentes
tweetCorpus <- tm_map(tweetCorpus, removeWords,c('oscars','oscar','pelicula','peliculas','eduaubcedubeuau','eduaubdedubuf','eduaubcedubeua','eduaubdedubud','eduaubdedubub','eduaubdedubu'))

tdm <- TermDocumentMatrix(tweetCorpus)

dataF <- toDataFrame(tdm)

wordcloud(dataF$word, dataF$freq, min.freq = 5,random.order=FALSE, colors=brewer.pal(8, "Set2"))

##############################TOPIC MODEL####################################
dtm <- as.DocumentTermMatrix(tdm) #transformamos la matros documentos-terminos

SEED <- sample(1:1000000, 1)  # utilizamos un seed random
k <- 10 #numero de topicos


models <- list(
  CTM       = CTM(dtm, k = k, control = list(seed = SEED, var = list(tol = 10^-4), em = list(tol = 10^-3))),
  VEM       = LDA(dtm, k = k, control = list(seed = SEED)),
  VEM_Fixed = LDA(dtm, k = k, control = list(estimate.alpha = FALSE, seed = SEED)),
  Gibbs     = LDA(dtm, k = k, method = "Gibbs", control = list(seed = SEED, burnin = 1000,
                                                               thin = 100,    iter = 1000))
)


#se revisan los primeros 4 terminos por topico
lapply(models, terms, 4)

Gibbs <- LDA(dtm, k = k, method = "Gibbs", control = list(seed = SEED, burnin = 1000,thin = 100,    iter = 1000))
terms(Gibbs,4)

topicmodels_json_ldavis <- function(fitted, corpus, doc_term){
  # Required packages
  

  ## Find required quantities
  phi <- posterior(fitted)$terms %>% as.matrix
  theta <- posterior(fitted)$topics %>% as.matrix
  vocab <- colnames(phi)
  doc_length <- vector()
  for (i in 1:length(corpus)) {
    temp <- paste(corpus[[i]]$content, collapse = ' ')
    doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
  }
  #temp_frequency <- inspect(dtm)
  #freq_matrix <- data.frame(ST = colnames(temp_frequency),
 #                           Freq = colSums(temp_frequency))
#  rm(temp_frequency)
  
  
  freq_matrix <- toDataFrame(doc_term)
  
  # Convert to json
  json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                 vocab = vocab,
                                 doc.length = doc_length,
                                 term.frequency = freq_matrix$freq)
  
  return(json_lda)
}
##################################################################

lda_json <- topicmodels_json_ldavis(Gibbs,tweetCorpus,tdm)
serVis(lda_json)
