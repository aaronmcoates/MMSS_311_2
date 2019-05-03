setwd("~/Documents/GitHub/MMSS_311_2")

library(xml2)
library(rvest)

hello <- read_html('/Users/aaroncoates/Desktop/cries.html', skip=0)

nodes <- html_nodes(hello, 'li a')

text <- html_text(nodes)
hrefy <- html_attr(nodes, "href")

papi <- cbind(text, href)
slay <- as.data.frame(papi)

newframe <- slay[3:12,]

read_html(newframe$hrefy)