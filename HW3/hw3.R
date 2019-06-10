setwd("~/Documents/GitHub/MMSS_311_2")

library(tidytext)
library(tm)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(proxy)
library(fields)
library(mixtools)

manifestos <- read_csv("/Users/aaroncoates/Downloads/manifestos.csv")

tidymani <- manifestos %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  mutate(word = removeNumbers(word)) %>%
  mutate(word = stripWhitespace(word)) %>%
  mutate(word = removePunctuation(word)) %>%
  mutate(word = stemDocument(word)) %>%
  group_by(doc_id) %>%
  count(word) %>%
  subset(word != '') %>%
  cast_dtm(doc_id, word, n) %>%
  removeSparseTerms(0.99) %>%
  as.matrix()


removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x)

corpus3 <- VCorpus(DataframeSource(manifestos))
corpus4 <- corpus3 %>% 
  tm_map(removePunctuation) %>%
  tm_map(content_transformer(removeSpecialChars)) %>%
  tm_map(content_transformer(removeNumbers)) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(content_transformer(stemDocument), language = "english")
  

dtm <- DocumentTermMatrix(corpus4)
dtm <- removeSparseTerms(dtm, 0.97)

dtm <- dtm %>%
  as.matrix() %>%
  tidy()

manifestos2 <- read.csv("/Users/aaroncoates/Downloads/manifestos.csv")
manifestos2[2,2]


eucdist.mani <- dist(tidymani) %>%
  as.matrix()

manifestosabc <- manifestos[order(manifestos$doc_id),]

image.plot(1:ncol(eucdist.mani), 1:ncol(eucdist.mani), eucdist.mani, axes=F, xlab="Party", ylab="Party")
  axis(1, 1:ncol(eucdist.mani), manifestosabc$doc_id, cex.axis=.7, las=2)
  axis(2, 1:ncol(eucdist.mani), manifestosabc$doc_id, cex.axis=.7, las=2)
  
cosdist.mani <- dist(tidymani, method="cosine") %>%
  as.matrix()
  
image.plot(1:ncol(cosdist.mani), 1:ncol(cosdist.mani), cosdist.mani, axes=F, xlab="Party", ylab="Party")
axis(1, 1:ncol(cosdist.mani), manifestosabc$doc_id, cex.axis=.7, las=2)
axis(2, 1:ncol(cosdist.mani), manifestosabc$doc_id, cex.axis=.7, las=2)

sanny <- read_csv("/Users/aaroncoates/Downloads/311_sanitation_requests_2019.csv")
sanny <- sanny[!is.na(sanny$Latitude), ]
sanny <- sanny[!is.na(sanny$Longitude), ]

set.seed(80)
kmeanmat<- kmeans(select(sanny, c(Latitude, Longitude)), 2, nstart=50)
clusty <- kmeanmat$cluster
sanny$cluster <- as.factor(clusty)

table(sanny$`Borough`, sanny$cluster)

ggplot(sanny, aes(x=Latitude, y=Longitude)) + geom_point(alpha=.1, size=.5, col= sanny$cluster)

centroidz <- as.data.frame(kmeanmat$centers)

ggplot(sanny, aes(x=Latitude, y=Longitude, color=sanny$cluster)) + geom_point(alpha=.1, size=.5) + 
  scale_color_manual(values=c("#E0A4FF", "#1FD0FA"), guide=FALSE) +
  geom_point(data=centroidz, aes(Latitude, Longitude), color='white', size=5) +
  geom_point(data=centroidz, aes(Latitude, Longitude), color='black', size=4) +
  theme_bw()
  
kmeanmat3 <- kmeans(select(sanny, c(Latitude, Longitude)), 3, nstart=50)
clusty3 <- kmeanmat3$cluster
sanny$cluster3 <- as.factor(clusty3)
table(sanny$`Borough`, sanny$cluster3)
ggplot(sanny, aes(x=Latitude, y=Longitude)) + geom_point(alpha=.1, size=.5, col= sanny$cluster3)
centroidz3 <- as.data.frame(kmeanmat3$centers)
ggplot(sanny, aes(x=Latitude, y=Longitude, color=sanny$cluster3)) + geom_point(alpha=.1, size=.5) + 
  scale_color_manual(values=c("#5FEE62", "#1FD0FA", "#F06ED6"), guide=FALSE) +
  geom_point(data=centroidz3, aes(Latitude, Longitude), color='white', size=5) +
  geom_point(data=centroidz3, aes(Latitude, Longitude), color='black', size=4) +
  theme_bw()

sumsq <- numeric(15)
for (i in 1:15){
  km <- kmeans(select(sanny, c(Latitude, Longitude)), i, nstart=15)
  sanny[[paste0("k", i)]] <- as.factor(km$cluster)
  sumsq[i] <- km$tot.withinss
} 

sumsq<- as.data.frame(sumsq)
sumsq$k <- c(1:15)
ggplot(sumsq, aes(k, sumsq)) + geom_line() + geom_point()

table(sanny$Borough, sanny$k4)

kmeanmat4 <- kmeans(select(sanny, c(Latitude, Longitude)), 4, nstart=50)
clusty4 <- kmeanmat4$cluster
sanny$cluster4 <- as.factor(clusty4)
centroidz4 <- as.data.frame(kmeanmat4$centers)
ggplot(sanny, aes(x=Latitude, y=Longitude, color=sanny$cluster4)) + geom_point(alpha=.1, size=.5) + 
  scale_color_manual(values=c("#5FEE62", "#1FD0FA", "#F06ED6", "#FFC034"), guide=FALSE) +
  geom_point(data=centroidz4, aes(Latitude, Longitude), color='white', size=3) +
  geom_point(data=centroidz4, aes(Latitude, Longitude), color='black', size=2) +
  theme_bw()

final <- sanny[sample(1:nrow(sanny), 500, replace=FALSE),]
plot(mvnormalmixEM(select(final, c(Latitude, Longitude)), k=2), whichplots=2, xlab2="Latitude", ylab2="Longitude")

plot(mvnormalmixEM(select(final, c(Latitude, Longitude)), k=3), whichplots=2, xlab2="Latitude", ylab2="Longitude")


