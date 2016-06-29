# Author: Pallavi Karan
# Date: 02/11/2016
# Purpose: Classififcation & ReSampling

# Validation Set Approach
rm(list=ls())
library(ISLR)
attach(Default)
summary(Default)
N<- nrow(Default)
train <- sample(N,N/2)


#Estimating logestic regression
glm.fit<-glm(default~ balance + income, family=binomial, subset=train)

#Estimate predicted probabilities
pred.probs <-predict(glm.fit,Default[-train,], type="response")

#Classify observations using a 0.5 threshold
pred.default <- rep("No",N/2)
pred.default[pred.probs > 0.5] <-"Yes"

#Calculate the misclassification rate
error.rate <- mean(pred.default !=default[-train])

#k-fold Cross-Validation
library(boot)
mycost <- function(r,pi=0){
  mean(abs(r-pi) >0.5) 
  }
# cv.glm(Default, glm.fit, cost=mycost ,K=10)
cv.error.10 <- cv.glm(Default, glm.fit, cost=mycost, K=10)
cv.error.10$delta


# Leave-One-Out Cross Validation 

loocv.error <- cv.glm(Default, glm.fit, cost, K=nrow(Default))
