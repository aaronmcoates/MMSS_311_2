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
library(topicmodels)
library(stm)

set.seed(732)
Ari <- read_csv("/Users/aaroncoates/Downloads/tx_deathrow_full.csv")
Ari <- Ari[is.na(Ari$'Last Statement')!=1, ]

Taylor <- Ari %>% 
  unnest_tokens(word, 'Last Statement') %>%
  anti_join(stop_words) %>%
  mutate(word = removePunctuation(word)) %>%
  group_by(Execution) %>%
  count(word) %>%
  cast_dtm(Execution, word, n) %>%
  as.matrix()

Britney <- LDA(Taylor, k=10)
Kelly <- Britney %>%
  tidy() %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup()

CarlyRae <- ggplot(Kelly, aes(term, beta, fill= as.factor(topic))) +
  facet_wrap(~ topic, scales= 'free_y', nrow = 2) + coord_flip() + 
  geom_col(show.legend = FALSE) + xlab('Term') + ylab('Beta') +
  labs(title="Topic Modeling of Death Row Final Statements, LDA", subtitle="Top 10 Words Per Topic")
CarlyRae

Beyonce <- stm::readCorpus(Taylor, type = 'dtm')
Normani <- as.data.frame(Taylor)
Normani$ExecutionNumber <- as.numeric(rownames(Normani))
Camila <- semi_join(Ari, Normani, by = c('Execution' = 'ExecutionNumber'))

Adele <- stm(documents = Beyonce$documents, vocab = Beyonce$vocab,
               K = 10, prevalence = ~ Race, data = Camila)

labelTopics(Adele)
plot.STM(Adele, type = "summary", n=5, xlim=c(0,1))
summary(Adele)

Miley <- tidy(Adele) %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup()
Demi <- ggplot(Miley, aes(term, beta, fill= as.factor(topic))) +
  facet_wrap(~ topic, scales= 'free_y', nrow = 2) + coord_flip() + 
  geom_col(show.legend = FALSE) + xlab('Term') + ylab('Beta') +
  labs(title="Topic Modeling of Death Row Final Statements, STM", subtitle="Top 10 Words Per Topic")
Demi

