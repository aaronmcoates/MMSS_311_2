setwd("~/Documents/GitHub/MMSS_311_2")

TwitterData <- read.csv('/Users/aaroncoates/Downloads/trumptweets.csv')
library(tidytext)
library(tm)
library(dplyr)
library(broom)
library(lubridate)
library(stringr)
library(ggplot2)
library(tidyr)
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
htcorpus2 <- tm_map(hashtagcorpus, content_transformer(hashtag)) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(content_transformer(tolower))

htDTMatrix <- DocumentTermMatrix(htcorpus2)
tidyhash <- tidy(htDTMatrix)

popht <- tidyhash %>%
  group_by(term) %>%
  summarize(frequency = sum(count)) %>%
  arrange(desc(frequency))

hashtagdaddy <- subset(popht, grepl("#", term))
hashtagdaddy[1:5, 1:2]

DateHashtag <- tidyhash %>%
  subset(term == '#maga' | term == '#trump2016'
         | term == '#celebapprentice' | term == '#celebrityapprentice'
         | term == '#makeamericagreatagain')

x <- 1:17200
TwitterData$document <- x
TwitterData$document <- as.character(TwitterData$document)

final <- inner_join(DateHashtag, TwitterData, by = 'document')
final <- final[, c('document', 'term', 'created_at', 'count')]

final$date <- as.Date(final$created_at, '%m-%d-%Y')
final$month <- format(final$date, '%Y-%m')

maga <- final %>%
  group_by(month, term) %>%
  summarise(frequency = sum(count))

maga <- arrange(maga, month)

all_dates2 = seq(as.Date(as.yearmon(min(maga$month))), as.Date(as.yearmon(max(maga$month))), by="month")
date2 = as.data.frame(all_dates2)
date2$month <- format(date2$all_dates, '%Y-%m')

posts_by_date_clean2 = merge(date2,
                            maga,
                            by.x='month',
                            by.y='month',
                            all.x=T,
                            all.y=T)

posts_by_date_clean2$frequency[is.na(posts_by_date_clean2$frequency)] = 0
posts_by_date_clean2$term[is.na(posts_by_date_clean2$term)] = '#trump2016'


ggplot(data=posts_by_date_clean2, aes(x=month, y=frequency, group = term, colour = term)) +
  geom_line() +
  geom_point(size=4, shape=21, fill="white") + 
  scale_y_continuous(limits = c(1, 100)) + coord_flip()

#rmarkdown size

Crooked <- TwitterData
Crooked <- select(Crooked, c(text, created_at)) %>%
  unnest_tokens(bigram, text, token='ngrams', n=2)
Crooked$date <- as.Date(Crooked$created_at, '%m-%d-%Y')
Crooked$month <- format(Crooked$date, '%Y-%m')

Crooked <- subset(Crooked, bigram=='crooked hillary')

for (i in 1:nrow(Crooked)) {
  Crooked$count[i] =1
}

install.packages('zoo')
library(zoo)


finalcrooked <- Crooked %>%
  group_by(month) %>%
  summarise(frequency = sum(count))

all_dates = seq(as.Date(as.yearmon(min(finalcrooked$month))), as.Date(as.yearmon(max(finalcrooked$month))), by="month")
date = as.data.frame(all_dates)
date$month <- format(date$all_dates, '%Y-%m')

posts_by_date_clean = merge(date,
                            finalcrooked,
                            by.x='month',
                            by.y='month',
                            all.x=T,
                            all.y=T)

posts_by_date_clean$frequency[is.na(posts_by_date_clean$frequency)] = 0

ggplot(data=posts_by_date_clean, aes(x=month, y=frequency)) +
  geom_col(color='blue', fill='red') 



