setwd("~/Documents/GitHub/MMSS_311_2")

x <- c(1, 2, 3, 4, 5)

Mindy <- 12

slay <- matrix(1:6, nrow=2, ncol=3, byrow=TRUE)

queen <- matrix(1:6, nrow=2, ncol=3)

yas <- matrix(1, nrow=10, ncol=10)

vector <- c("THIS", "IS", "A", "VECTOR")

sum3 <- function(x,y,z) {
  x + y + z
}
sum3(1,2,3)

function2 <- function(x) {
  if(x<=10) print("YES")
  if(x>10) print("NO")}
function2(6)
function2(10)
function2(13)

g <- rnorm(1000, mean=10, sd=1)

y <- rnorm(1000, mean=5, sd=.5)

x <- vector()
for (i in 1:1000) {
  x[i] <- mean(sample(g, size=10, replace=TRUE, prob=NULL))
}

regreg <- lm(y ~ x)
summary (regreg)