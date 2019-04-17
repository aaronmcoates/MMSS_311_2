setwd("~/Documents/GitHub/MMSS_311_2")
pol <- read.csv("/Users/aaroncoates/Downloads/pol_data.csv")

library(broom)
library(e1071)
library(caret)

pol$group <- as.factor(pol$group)
trainsize <- floor((2/3)*nrow(pol))
set.seed(100)
train_pol <- sample(nrow(pol), size = trainsize, replace=FALSE)
trainingdata <- pol[train_pol, ]
testydata <- pol[-train_pol, ]

tunez <- tune(svm, group ~ pol_margin + col_degree + house_income, data=trainingdata, kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
swaggysvm <- tunez$best.model
summary(swaggysvm)
svmpredict <- predict(swaggysvm, testydata)
svmpredict
svmtable <- table("Prediction"=svmpredict, "True Party"=testydata[,1])
svmtable

naive <- naiveBayes(group ~ pol_margin + col_degree + house_income, data=trainingdata)
naivepred <- predict(naive, testydata)
naivepred
naivetable <- table("Prediction"=naivepred, "True Party"=testydata[,1])
naivetable
