setwd("~/Documents/GitHub/MMSS_311_2")
wid <- read.csv("/Users/aaroncoates/Downloads/widget_data.csv")

library(dplyr)
library(ggplot2)
library(glmnet)
library(broom)
library(aod)

grid=10^seq(2,-2,length=100)
ridge <- glmnet(x = as.matrix(wid[, -1]), y = wid$y, alpha=0, lambda=grid)
plot(ridge, label = TRUE)

ridge1 <- tidy(ridge)
ridge2 <- ridge1[ which(ridge1$term!='(Intercept)'), ]
ggplot(data=ridge2, aes(x=lambda, y=estimate, colour=term)) + 
  geom_line() + coord_cartesian(xlim = c(0,100), ylim = c(-.5, .5))

crossval <- cv.glmnet(x = as.matrix(wid[, -1]), y = wid$y, alpha=0, lambda=grid)
plot(crossval)

crossval$lambda.min
coef(crossval, s = "lambda.min")

grid=10^seq(2,-2,length=100)
lasso <- glmnet(x = as.matrix(wid[, -1]), y = wid$y, alpha=1, lambda=grid)
plot(lasso, label = TRUE)

lasso1 <- tidy(lasso)
ggplot(data=lasso1, aes(x=lambda, y=estimate, colour=term)) + 
  geom_line() + coord_cartesian(xlim = c(0,2), ylim = c(-1, 1))

crossvallasso <- cv.glmnet(x = as.matrix(wid[, -1]), y = wid$y, alpha=1, lambda=grid)
plot(crossvallasso)

crossvallasso$lambda.min
coef(crossvallasso, s = "lambda.min")


install.packages("rmarkdown")
library(rmarkdown)