setwd("~/Documents/GitHub/MMSS_311_2")
pums <- read.csv("/Users/aaroncoates/Downloads/HW0-master/pums_chicago.csv")

print(dim(pums))

mean(pums$PINCP, na.rm=TRUE)

pums$logpincp <- (log(pums$PINCP))

pums$GRAD.DUMMY <- ifelse(pums$SCHL>=18, "grad", "no grad")

library(dplyr)
byebye <- select(pums, -c(SERIALNO))

write.csv(byebye, file = "newdata.csv")

under16 <- subset(byebye, ESR=="b")
employed <- subset(byebye, ESR %in% c(1,2))
unemployed <- subset(byebye, ESR==3)
af <- subset(byebye, ESR %in% c(4,5))
notlf <- subset(byebye, ESR==6)

employedAF <- rbind(employed, af)
employedAF= employedAF[c("AGEP", "RAC1P", "logpincp")]

pums2 <- read.csv("/Users/aaroncoates/Downloads/HW0-master/pums_chicago.csv")

mean(pums2$JWMNP, na.rm=TRUE)
median(pums2$JWMNP, na.rm=TRUE)
quantile(pums2$JWMNP, na.rm=TRUE, .8)

cor(pums2$JWMNP, pums2$WAGP, use="complete.obs")

pums2$logpincp <- (log(pums2$PINCP))

library(ggplot2)

pdf(file="graphygraph.pdf")
plot(pums2$AGEP, pums2$logpincp, xlab="AGE", ylab="LOG INCOME", col = "purple")
dev.off()

tablehaha <- table(pums2$ESR, pums2$RAC1P)
tablehaha

cutereg <- lm(pums2$WAGP ~ pums2$WKHP)
summary(cutereg)

cuteresid <- resid(cutereg)
cutefit <- fitted(cutereg)

plot(cutefit, cuteresid, ylab="Residuals", xlab="Fitted Values", col = "turquoise")

data("mtcars")
head(mtcars)

pretty <- lm(mtcars$mpg ~ mtcars$wt)
summary(pretty)

automatic <- subset(mtcars, am==0)
automaticreg <- lm(automatic$mpg ~ automatic$wt)
summary(automaticreg)

man <- subset(mtcars, am==1)
manreg <- lm(man$mpg ~ man$wt)
summary(manreg)

mtcars$loghp <- log(mtcars$hp)
hittheneighneigh <- lm(mtcars$mpg ~ mtcars$loghp)
summary(hittheneighneigh)

library(ggplot2)
mtcars$transmission <- ifelse(mtcars$am>0, "manual", "automatic")
mtcars$forwardgears <- ifelse(mtcars$gear>4, "five", ifelse(mtcars$gear>3 & mtcars$gear<=4, "four", "three"))

ggplot(mtcars, aes(wt, mpg, col=transmission, shape=forwardgears)) + geom_point() +
  scale_color_manual(values=c("black", "blue")) + xlab("weight") + ylab("miles per gallon") + 
  labs(shape="gears") + theme(panel.background = element_rect(fill="#C6B7FF"))
