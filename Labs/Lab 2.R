setwd("~/Documents/GitHub/MMSS_311_2")

#1
odev <- function(x){
  ifelse(round(x, digits=0) - x!=0, "Not an integer", ifelse(round(x/2, digits=0) - x/2 != 0, "Odd", "Even"))
}
  
odev(11)
odev(103.5)
odev(26)

hey <- function(x){
  if(round(x, digits=0) - x!=0){
    print("Not an Integer!")
  } else if(round(x/2, digits=0) - (x/2) != 0) {
    print("Odd")
  } else {
    print("Even")
  }}

hey(5)
hey(62)
hey(34.5)

#2
vecky <- c(1:6)
slayme <- c(sample(vecky, 20, replace = TRUE))

final <- sapply(slayme, 
                function(x){
                  if(round(x/2, digits=0) - x/2 != 0){
                  2 * x
                } else {
                  x^2
                }})

#3
library(dplyr)
data("starwars")
head(starwars)

print(dim(starwars))

mean(starwars$mass, na.rm=TRUE)

mean(starwars$mass[starwars$eye_color=="blue" & 160 < starwars$height & 200 > starwars$height], na.rm=TRUE)

starwarz <- subset(starwars, starwars$eye_color=="blue" & 160 < starwars$height & 200 > starwars$height)

newstars <- subset(starwars, select = c(name, height, mass, gender, species))

plot(newstars$mass, newstars$height)

newstars2 <- newstars[-c(16),]

regression <- lm(newstars2$mass ~ newstars2$height)
summary(regression)

y <- newstars2$mass
x <- newstars2$height
reg <- lm(y ~ x)
predict(reg, data.frame(x = 180))