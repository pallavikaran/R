rm(list=ls())

library(ISLR)
#Author: Pallavi Karan
#Date: 03/03/2016
#Purpose: Demonstrate apply functions(Used to avoid for loop coz of too mych memory usage)

ads <- read.csv("http://www-bcf.usc.edu/~gareth/ISL/Advertising.csv")
print(ads)
budget <- ads[, c("TV","Radio","Newspaper")]

#compute row means
apply(budget, 1, mean) # 1 s for row
apply(budget, 1, function(x) mean(x))
apply(budget, 1, function(x) sum((x-mean(x))^2))

#Compute column means
apply(budget, 2, mean) # 2 is or column 

attach(Wage)
summary(Wage)

#Compute avergae salary by education 
by(wage, education, mean) #(obj, vector, function)

#apply functions to a list
fit <- list()  #Declaration

#fit[[1]] to check the value in the list ith index

for (i in 1:4){
  fit[[i]] <- lm(wage~ poly(age,i))
}

fit[[1]]

lapply(fit,summary)

summary(fit[[1]])$adj.r.squared # for one

lapply(fit, function(x) summary(x)$adj.r.squared) # for multiple 
unlist(lapply(fit, function(x) summary(x)$adj.r.squared)) #For treating as vectors
sapply(fit, function(x) summary(x)$adj.r.squared) #For treating as vectors
