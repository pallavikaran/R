# Author: Pallavi Karan
# Date: 02/09/2016
# Purpose: Classification with logistic reression in R
rm(list=ls())
library(ISLR)
attach(Default)
summary(Default)

m.balance <- glm(default ~ balance, family=binomial)
summary(m.balance)

pred.data <-data.frame(balance=c(1000,2000,3000))
pred.data
predict(m.balance, pred.data, type="response")

m<- glm(default ~ balance + income +student, family=binomial)
summary(m)

pred.data <- data.frame(balance=c(1000,2000,3000), income=1000, student="No")
summary(pred.data)
predict(m, pred.data, type="response")


# Classification and confusion matrix

pred.probs <- predict(m, type="response")
length(pred.probs) #10000 obeservations
pred.default <- rep("No", nrow(Default))
pred.default[pred.probs >0.5] <- "Yes"
table(pred.default)

confusion.matrix <- table(default, pred.default)
confusion.matrix
addmargins(confusion.matrix)
