setwd("~/Documents/GitHub/MMSS_311_2")

TwitterData <- read.csv('/Users/aaroncoates/Downloads/trumptweets.csv')

library(tidytext)
library(tm)
library(dplyr)
library(broom)
library(lubridate)
library(stringr)

tweetcorpus <- Corpus(VectorSource(as.vector(TwitterData$text)))

processedcorpus <- tweetcorpus %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(content_transformer(stemDocument), language = "english") %>%
  tm_map(content_transformer(removePunctuation))

DTMatrix <- DocumentTermMatrix(processedcorpus)
SparseDTMatrix <- removeSparseTerms(DTMatrix, .99)
inspect(SparseDTMatrix[1:10,1:10])
tidymatrix <- tidy(PDTMatrix)

TfIdfMat <- DocumentTermMatrix(processedcorpus, control
                          = list(weighting = weightTfIdf))
SparseTfIdfMat <- removeSparseTerms(TfIdfMat, .99)
inspect(SparseTfIdfMat[1:10,1:10])

popterms <- tidymatrix %>%
  group_by(term) %>%
  summarize(frequency = sum(count)) %>%
  arrange(desc(frequency))
popterms[1:20,1:2]

TwitterDataDate <- TwitterData
TwitterDataDate$date <- as.Date(TwitterData$created_at, '%m-%d-%Y')

PostTwitterData <- subset(TwitterDataDate, date >= as.Date('2016-11-08'))
PreTwitterData <- subset(TwitterDataDate, date <= as.Date('2016-11-08'))

#post election
posttweetcorpus <- Corpus(VectorSource(as.vector(PostTwitterData$text)))

postprocessedcorpus <- posttweetcorpus %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(content_transformer(stemDocument), language = "english") %>%
  tm_map(content_transformer(removePunctuation))

postDTMatrix <- DocumentTermMatrix(postprocessedcorpus)
postSparseDTMatrix <- removeSparseTerms(postDTMatrix, .99)
inspect(postSparseDTMatrix[1:10,1:10])
posttidymatrix <- tidy(postDTMatrix)

postpopterms <- posttidymatrix %>%
  group_by(term) %>%
  summarize(frequency = sum(count)) %>%
  arrange(desc(frequency))
postpopterms[1:20,1:2]

#pre election
pretweetcorpus <- Corpus(VectorSource(as.vector(PreTwitterData$text)))

preprocessedcorpus <- pretweetcorpus %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(content_transformer(stemDocument), language = "english") %>%
  tm_map(content_transformer(removePunctuation))

preDTMatrix <- DocumentTermMatrix(preprocessedcorpus)
preSparseDTMatrix <- removeSparseTerms(preDTMatrix, .99)
inspect(preSparseDTMatrix[1:10,1:10])
pretidymatrix <- tidy(preDTMatrix)

prepopterms <- pretidymatrix %>%
  group_by(term) %>%
  summarize(frequency = sum(count)) %>%
  arrange(desc(frequency))
prepopterms[1:20,1:2]

#hashtags
hashtagcorpus <- Corpus(VectorSource(as.vector(TwitterData$text)))

hashtag <- function(x) gsub("[^#[:alnum:][:space:]]", "", x)
htcorpus2 <- tm_map(hashtagcorpus, content_transformer(hashtag))

htDTMatrix <- DocumentTermMatrix(htcorpus2)
tidyhash <- tidy(htDTMatrix)

popht <- tidyhash %>%
  group_by(term) %>%
  summarize(frequency = sum(count)) %>%
  arrange(desc(frequency))

hashtagdaddy <- subset(popht, grepl("#", term))
hashtagdaddy[1:5, 1:2]

Top5 <- Top5[ ,c('dates1', 'document')]
for (i in 1:nrow(Top5)) {
Top5$maga[i] <- Top5Baby %>%
  filter(term == 'maga') %>%
  filter(document < Top5$document[i]) %>%
  sum(count)
}




Cries <- subset(TwitterData, grepl("#maga", text))

Top5Baby <- subset(tidyhash, term == '#trump2016' | term == '#makeamericagreatagain' 
                   | term == '#celebapprentice' | term == '#celebrityapprentice'
                   | term == '#maga')

dates1 <- TwitterData$created_at
Top5 <- as.data.frame(dates1)
Top5$realdate <- as.Date(Top5$dates1, '%m-%d-%Y')
Top5$document <- c(1:17200)
Top5 <- Top5[!duplicated(Top5$realdate), ]

Top555 <- Top5Baby %>%
  group_by(term) %>%
  summarize(frequency = sum(count)) %>%
  arrange(desc(frequency))

Top5$maga[i] <- 



Top6Baby <- subset(TwitterData, grepl('#trump2016', text) | grepl('#makeamericagreatagain', text) |
                                        grepl('#celebapprentice', text) | grepl('#celebrityapprentice', text)
                   | grepl('#maga', text))