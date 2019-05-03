setwd("~/Documents/GitHub/MMSS_311_2")

library(xml2)
library(rvest)
library(tm, tidytext)
library(stringr)

no <- read_html('/Users/aaroncoates/Desktop/cries.html')
nodes <- html_nodes(no, '.mw-category-group+ .mw-category-group a')

country <- html_text(nodes)
url <- html_attr(nodes, "href")
fullurl <- url_absolute(url, 'https://en.wikipedia.org/wiki/Category:Member_states_of_the_Association_of_Southeast_Asian_Nations')
fullurl

combined <- cbind(country, fullurl)
finaldata <- as.data.frame(combined, stringsAsFactors = F)

for(i in 1:10){
  finaldata$text[i] <- finaldata$fullurl[i] %>%
    read_html() %>%
    html_nodes('p+ ul li , p') %>%
    html_text() %>%
  paste(collapse = ' ')
}