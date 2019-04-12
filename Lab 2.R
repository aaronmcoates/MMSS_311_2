setwd("~/Documents/GitHub/MMSS_311_2")

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


vecky <- c(1:6)
slayme <- c(sample(vecky, 20, replace = TRUE))

final <- sapply(slayme, 
                function(x){
                  if(round(x/2, digits=0) - x/2 != 0){
                  2 * x
                } else {
                  x^2
                }})

