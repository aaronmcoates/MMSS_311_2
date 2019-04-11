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

