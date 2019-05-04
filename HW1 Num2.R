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

processedhashtagcorpus <- hashtagcorpus %>%
  tm_map(content_transformer(gsub), "[^\\#[:alnum:]\\s]", "")

htDTMatrix <- DocumentTermMatrix(processedhashtagcorpus)
tidy(htDTMatrix)