setwd("~/Documents/GitHub/MMSS_311_2")

library(xml2)
library(rvest)
library(tm, tidytext)
library(stringr)

WebPage <- read_html("https://pitchfork.com/reviews/albums/?genre=pop&page=1")

PitchforkLink <- html_nodes(WebPage, ".review__link") %>%
  html_attr("href")
PitchforkLink <- url_absolute(PitchforkLink, "https://pitchfork.com/reviews/albums/?genre=pop&page=1")
PitchforkLink

ArtistTitle <- html_nodes(WebPage, ".review__title-artist li") %>%
  html_text
ArtistTitle

AlbumTitle <- html_nodes(WebPage, ".review__title-album") %>%
  html_text
AlbumTitle

ADF <- cbind(AlbumTitle, ArtistTitle, PitchforkLink) %>%
  as.data.frame(stringsAsFactors = F)

for(i in 1:12){
  ADF$ReviewText[i] <- ADF$PitchforkLink[i] %>%
    read_html() %>%
    html_nodes('.row .clearfix p') %>%
    html_text() %>%
    paste(collapse = ' ')
}

for(i in 1:12){
  ADF$Score[i] <- ADF$PitchforkLink[i] %>%
    read_html() %>%
    html_nodes('.score') %>%
    html_text() 
}


Pages <- seq(1, 85)
EachPage <- paste0('https://pitchfork.com/reviews/albums/?genre=pop&page=', Pages)
EachPage

CycleThrough <- function(x) {
  fixed <- html_session(x)
  PitchforkLink <- html_nodes(fixed, ".review__link") %>%
    html_attr("href")
    PitchforkLink <- url_absolute(PitchforkLink, "https://pitchfork.com/reviews/albums/?genre=pop&page=1")
  AlbumTitle <- html_nodes(fixed, ".review__title-album") %>%
    html_text
  ADF <- cbind(AlbumTitle, PitchforkLink) %>%
    as.data.frame(stringsAsFactors = F)
}

DF <- lapply(EachPage, CycleThrough)

finaldf <- do.call(rbind, DF)           

for(i in 1:600){
  ADF$ReviewText[i] <- ADF$PitchforkLink[i] %>%
    read_html() %>%
    html_nodes('.row .clearfix p') %>%
    html_text() %>%
    paste(collapse = ' ')
}

for(i in 1:600){
  finaldf$Score[i] <- finaldf$PitchforkLink[i] %>%
    read_html() %>%
    html_nodes('.score') %>%
    html_text() 
}


hello <- read.csv("/Users/aaroncoates/Downloads/Political-media-DFE.csv")



