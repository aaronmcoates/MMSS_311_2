setwd("~/Documents/GitHub/MMSS_311_2")

library(xml2)
library(rvest)

no <- read_html('/Users/aaroncoates/Desktop/cries.html', skip=0)

nodes <- html_nodes(no, 'li a')

text <- html_text(nodes)
hrefy <- html_attr(nodes, "href")
hrefy2 <- url_absolute(hrefy, 'https://en.wikipedia.org/wiki/Category:Member_states_of_the_Association_of_Southeast_Asian_Nations')
hrefy2

papi <- cbind(text, hrefy2)
slay <- as.data.frame(papi, stringsAsFactors = F)

newframe <- slay[3:12,]

for(i in 1:10){
  newframe$whyohwhy[i] <- newframe$hrefy2[i] %>%
    read_html() %>%
    html_nodes('div p') %>%
    html_text() %>%
    paste(collapse = '\n')
}
  
  