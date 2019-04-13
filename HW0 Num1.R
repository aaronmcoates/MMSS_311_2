setwd("~/Documents/GitHub/MMSS_311_2")

#1.(a)
x <- c(1, 2, 3, 4, 5)

#1.(b)
Mindy <- 12

#1.(c)
matrix1 <- matrix(1:6, nrow=2, ncol=3, byrow=TRUE)

#1.(d)
matrix2 <- matrix(1:6, nrow=2, ncol=3)

#1.(e)
matrix3 <- matrix(1, nrow=10, ncol=10)

#1.(f)
vector <- c("THIS", "IS", "A", "VECTOR")

#1.(g)
sum3 <- function(x,y,z) {
  x + y + z
}
sum3(1,2,3)

#1.(h)
function2 <- function(x) {
  if(x<=10) print("YES")
  if(x>10) print("NO")}
function2(6)
function2(10)
function2(13)

#1.(i)
g <- rnorm(1000, mean=10, sd=1)

#1.(j)
y <- rnorm(1000, mean=5, sd=.5)

#1.(k)
x <- vector()
for (i in 1:1000) {
  x[i] <- mean(sample(g, size=10, replace=TRUE, prob=NULL))
}

#1.(l)
regreg <- lm(y ~ x)
summary (regreg)
#
