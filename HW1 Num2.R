setwd("~/Documents/GitHub/MMSS_311_2")

milk <- read.csv('/Users/aaroncoates/Downloads/trumptweets.csv')

library(tidytext)
library(tm)
library(dplyr)

corpusiii <- Corpus(VectorSource(as.vector(milk$text)))

corpusiv <- corpusiii %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(content_transformer(stemDocument), language = "english") %>%
  tm_map(content_transformer(removePunctuation))

ahh <- DocumentTermMatrix(corpusiv)
ahh2 <- removeSparseTerms(ahh, .99)

inspect(ahh2[1:5,1:5])

library(broom)
ahh2 <- as.matrix(ahh2)

ahh3 <- tidy(ahh2)
ahh4 <- DocumentTermMatrix(corpusiv, control = list(weighting = weightTfIdf))
