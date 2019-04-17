setwd("~/Documents/GitHub/MMSS_311_2")
sick <- read.csv("/Users/aaroncoates/Downloads/sick_data.csv")

library(dplyr)
library(ggplot2)
install.packages("glmnet", repos = "http://cran.us.r-project.org")
library(glmnet)
install.packages("broom")
library(broom)
install.packages("aod")
library(aod)

sick$result.d <- sapply(sick$result, function(x){
  if(x == "Positive"){
    print(1)
  } else {
    print(0)
  }})

regOLS <- glm(result.d ~ temp + bp, data=sick) %>% broom::tidy()
for (i in 1:1000) {sick$predictionOLS[i] <- regOLS[1, 2] + regOLS[2, 2]*sick$temp[i] + regOLS[3,2]*sick$bp[i]}

sick$result <- factor(sick$result)
logit <- glm(result ~ temp + bp, data = sick, family = "binomial") %>% broom::tidy()
for (i in 1:1000) {sick$predictionLOGIT[i] <- logit[1, 2] + logit[2, 2]*sick$temp[i] + logit[3,2]*sick$bp[i]}

sum(with(sick, result.d==1 & predictionOLS>=.5))

sick$OLScorrect <- vector(1000L)
for (i in 1:1000){sick$OLScorrect[i] <-  if(sick$result.d[i] == 1 & sick$predictionOLS[i] >= .5){
  1
} else {
  0
}}
  
  
  {
    print(1)
  } else {
    print(0)
  }})

ridge <- glmnet(x = as.matrix(sick[, 2:3]), y = sick$result.d, alpha=0, lambda=1) %>% broom::tidy()
for (i in 1:1000) {sick$predictionRIDGE[i] <- ridge[1, 3] + ridge[2, 3]*sick$temp[i] + ridge[3,3]*sick$bp[i]}


