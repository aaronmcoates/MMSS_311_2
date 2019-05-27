setwd("~/Documents/GitHub/MMSS_311_2")

library(stats)
data(USArrests)

PCA <- prcomp(USArrests, center=TRUE, scale=TRUE)
print(PCA)

Scree <- plot(PCA, type="l")

library(devtools)
library(ggbiplot)

USA <- USArrests
USA$name <- row.names(USArrests)

p <- ggbiplot(PCA, obs.scale=1, var.scale=1, ellipse=TRUE, circle=TRUE) +
  theme_bw() +geom_text(aes(label=USA$name),hjust=0, vjust=0)
  
p