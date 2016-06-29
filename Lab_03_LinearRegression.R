# Author : Pallavi Karan
#Date: 01/28/2016
#Purpose: Linear regression in R

rm(list = ls()) #Clear the lists

library(MASS)
library(ISLR)

View(Boston)
names(Boston)
summary(Boston)
attach(Boston) # to attach the data set to the workspace

#visulaization
par(mfrow=c(1,2))
hist(medv)
hist(lstat)
plot(lm.fit)
boxplot(medv)
boxplot(lstat)

#training
# medv as target and lstat as predictor
y.hat <- 34.55 + -0.95 * c(5,10,15) #manual
y.hat <- lm.fit$coefficients[1] + lm.fit$coefficients[2] * c(5,10,15) #manual from workspace
# in console y.hat for output
lm.fit <- lm(medv ~ lstat)
plot(lm.fit)
lm.fit # gives ntercept and slope
summary(lm.fit)
confint(lm.fit) #estimate confidence

# testing
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval ="confidence")
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval ="prediction")


