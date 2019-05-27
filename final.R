setwd("/Users/aaroncoates/Documents/GitHub/MMSS_311_2")

library(tidytext)
library(tm)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(proxy)
library(fields)
library(mixtools)
library(xml2)
library(rvest)

num1 <- read_csv("/Users/aaroncoates/Desktop/1.csv")
num3 <- read_csv("/Users/aaroncoates/Desktop/3.csv")

hey <- inner_join(num1, num3, "FIPS Code")

hey3 <- read_html("http://en.wikipedia.org/wiki/User:Michael_J/County_table")
FIPS <- html_nodes(hey3, 'td:nth-child(3)')
LAT <- html_nodes(hey3, 'td:nth-child(13)')
LONG <- html_nodes(hey3, 'td:nth-child(14)')

'FIPS Code' <- html_text(FIPS)
LAT2 <- html_text(LAT)
LONG2 <- html_text(LONG)

final <- cbind(get('FIPS Code'), LAT2, LONG2)
final<- as.data.frame(final)
final$'FIPS Code' <- final$V1
final$'FIPS Code' <- as.numeric(as.character(final$'FIPS Code'))

hey10 <- inner_join(final, hey, 'FIPS Code')
hey10 <- subset(hey10, select = -c(V1, get('Per Capita Personal Income 2017'), Rank.x, Rank.y))

hey10$Data.x <- removePunctuation(hey10$Data.x)
hey10$Data.x <- as.numeric(hey10$Data.x)
hey10$Data.y <- as.numeric(hey10$Data.y)
hey10$'FIPS Code' <- as.character(hey10$`FIPS Code`)

hey2 <- kmeans(select(hey10, Data.x), 4, nstart=50)


hey10$cluster <- as.factor(hey2$cluster)
hey10$LAT2 <- gsub("°", "", hey10$LAT2, fixed = TRUE)
hey10$LONG2 <- gsub("°", "", hey10$LONG2, fixed = TRUE)
hey10$LAT2 <- as.numeric(hey10$LAT2)
hey10$LONG2 <- as.numeric(gsub("–", "-", hey10$LONG2, fixed = TRUE))

library(maps)
library(mapdata)
library(devtools)
library(ggmap)

usa <- map_data("usa")
counties <- map_data("county")
hey10$subregion <- as.character(tolower(gsub(" County, ..$", "", hey10$`Population 2018`)))
counties$subregion <- as.character(counties$subregion)

lol <- inner_join(counties, hey10, 'subregion')

ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3) + geom_point(data=hey10, aes(LONG2, LAT2, color=cluster), size=.3) + xlim(-125, -70) + ylim(20,60)


table(hey10$cluster)


ggplot(data = lol, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
  geom_polygon(color = "black", fill = "gray") +
  geom_polygon(data = lol, aes(fill = cluster), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() 
  
ggplot(data = counties, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")

