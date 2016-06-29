rm(list = ls())
library(ISLR) 

summary(Auto)
Auto
gas.median <-  median(Auto$mpg) # From Auto Dataset extracting mpg and calculating the median
binary.var <-  ifelse(Auto$mpg > gas.median, 1, 0) #a binary variable that takes on a 1 for cars with gas mileage above the median, and a 0 for cars with gas mileage below the median
print(binary.var)
Auto$newmpg <-  as.factor(binary.var) #Adding the binary.var column to Auto Dataset
Auto

#svm.model <- svm(binary.var ~ ., data = Auto, cost=1, gamma =1)
#svm.model
#svm.model <- svm(binary.var ~ ., data = Auto,ranges = list(cost=c(0.05, 0.5, 5, 10, 100)), gamma =seq(.5, .9, by = .1))
#svm.model

set.seed(1) # To reproduce exact same random numbers
library(e1071) # tune function in this library is used to perform cross validation
#Comparing SVM with Linear Kernal, Default cross validation with k=10 folds
tune.output <- tune(svm, newmpg ~ ., data = Auto, kernel = "linear", ranges = list(cost = c(0.01, 0.1, 1, 10, 100, 1000)), gamma =seq(.5, .9, by = .1))
#using the new binary.var for SVM as newmpg attached to Auto DS, Experimenting with different cost values
#cost is a function that specifies the error type to be calculated
summary(tune.output) #O/P shows cost of 1 seems to be the best parameter(lowest cross validation error rate) with error=0.01013231 as best performance
bestmod =tune.output$best.model
summary(bestmod)

set.seed(1)
#tune.output.poly <- tune(svm, newmpg ~ ., data = Auto, kernel = "polynomial", ranges = list(cost = c(0.01, 0.1, 1, 10, 100), degree = c(2, 3, 4), gamma=seq(.5,09, by =.1)))
tune.output.poly <- tune(svm, newmpg ~ ., data = Auto, kernel = "polynomial", ranges = list(cost = c(0.01, 0.1, 1, 10, 100), degree = c(2, 3, 4)))
summary(tune.output.poly) #O/P shows for cost 100 and degree 2, best performance is with error=0.2933333

tune.output.rad <- tune(svm, newmpg ~ ., data = Auto, kernel = "radial", ranges = list(cost = c(0.01, 0.1, 1, 10, 100), gamma = c(0.01, 0.1, 1, 10, 100)))
summary(tune.output.rad) #O/P shows for cost 100 and gamma 0.01, best performance is with error=0.01788462
#linear error(cost 1)=0.01013231 < Radial error(cost 100 & gamma 0.01)=0.01788462 < Polynomial error(cost 100 & degree 2)=0.2933333
#Amongst the three types of Kernel, Linear, Polynomial and Radial, so far linear has the best result with error of 0.01013231

#Training the ones with best performance with least errors from above and training 
svm.linear.fit <- svm(newmpg ~ ., data = Auto, kernel = "linear", cost = 1, scale =FALSE)
svm.linear.fit
svm.poly.fit <- svm(newmpg ~ ., data = Auto, kernel = "polynomial", cost = 100, degree = 2, scale =FALSE)
svm.poly.fit
svm.rad.fit <- svm(newmpg ~ ., data = Auto, kernel = "radial", cost = 100, gamma = 0.01,scale =FALSE)
svm.rad.fit


plotpairs <-  function(fit){
  for (name in names(Auto)[!(names(Auto) %in% c("mpg", "newmpg","name"))]) { #TRUE/False if the match in names(Auto) with C vector values 
    plot(fit, Auto, as.formula(paste("mpg~", name)))
  }
}
plotpairs(svm.linear.fit)
plotpairs(svm.poly.fit)
plotpairs(svm.rad.fit)



