#2.(a)
setwd("~/Documents/GitHub/MMSS_311_2")
pums <- read.csv("/Users/aaroncoates/Downloads/HW0-master/pums_chicago.csv")

#2.(b)
print(dim(pums))
#There are 204 variables.

#2.(c)
mean(pums$PINCP, na.rm=TRUE)

#2.(d)
pums$logpincp <- (log(pums$PINCP))
#NaN values are produced because some individuals have negative PINCP inputs, so it is impossible to take the log.

#2.(e)
pums$GRAD.DUMMY <- ifelse(pums$SCHL>=18, "grad", "no grad")

#2.(f)
library(dplyr)
bye <- select(pums, -c(SERIALNO))

#2.(g)
write.csv(bye, file = "newdata.csv")

#2.(h)
under16 <- subset(bye, ESR=="b")
employed <- subset(bye, ESR %in% c(1,2))
unemployed <- subset(bye, ESR==3)
af <- subset(bye, ESR %in% c(4,5))
notlf <- subset(bye, ESR==6)

#2.(i)
employedAF <- rbind(employed, af)

#2.(j)
employedAF= employedAF[c("AGEP", "RAC1P", "logpincp")]

#2.(k)
pums2 <- read.csv("/Users/aaroncoates/Downloads/HW0-master/pums_chicago.csv")

#2.(k)(i)
mean(pums2$JWMNP, na.rm=TRUE)
median(pums2$JWMNP, na.rm=TRUE)
quantile(pums2$JWMNP, na.rm=TRUE, .8)

#2.(k)(ii)
cor(pums2$JWMNP, pums2$WAGP, use="complete.obs")

#2.(k)(iii)(iv)
pums2$logpincp <- (log(pums2$PINCP))
library(ggplot2)
pdf(file="2kiii.pdf")
plot(pums2$AGEP, pums2$logpincp, xlab="AGE", ylab="LOG INCOME", col = "purple")
dev.off()

#2.(k)(v)
tablev <- table(pums2$ESR, pums2$RAC1P)
tablev

#2.(k)(vi)
regvi <- lm(pums2$WAGP ~ pums2$WKHP)
summary(regvi)

#2.(k)(vii)
cuteresid <- resid(regvi)
cutefit <- fitted(regvi)
plot(cutefit, cuteresid, ylab="Residuals", xlab="Fitted Values", col = "turquoise")
#This plot shows that there is not a linear relationship between wage and hours worked per week.
#This is because as the the fitted value increases, so does the error.
#If the relationship was linear, this plot would show a horizontal trend line at value zero.

#2.(l)(i)
data("mtcars")
head(mtcars)
regl <- lm(mtcars$mpg ~ mtcars$wt)
summary(regl)

#2.(l)(ii)
automatic <- subset(mtcars, am==0)
automaticreg <- lm(automatic$mpg ~ automatic$wt)
summary(automaticreg)

man <- subset(mtcars, am==1)
manreg <- lm(man$mpg ~ man$wt)
summary(manreg)

#2.(l)(iii)
mtcars$loghp <- log(mtcars$hp)
hittheneighneigh <- lm(mtcars$mpg ~ mtcars$loghp)
summary(hittheneighneigh)

#2.(m)
library(ggplot2)
mtcars$transmission <- ifelse(mtcars$am>0, "manual", "automatic")
mtcars$forwardgears <- ifelse(mtcars$gear>4, "five", ifelse(mtcars$gear>3 & mtcars$gear<=4, "four", "three"))
ggplot(mtcars, aes(wt, mpg, col=transmission, shape=forwardgears)) + geom_point() +
  scale_color_manual(values=c("black", "blue")) + xlab("weight") + ylab("miles per gallon") + 
  labs(shape="gears") + theme(panel.background = element_rect(fill="#C6B7FF"))