# Author: Pallavi Karan
# Date: Jan 14, 2016
# Purpose: Intruduction to R

x <- c(4,8,15,16,23,48)
length(x)
mean(x)
min(x)
max(x)
sd(x)
summary(x)

y <- c(8,27,34,4,19,10)
z <- c(1,2)

a <- x+ y
b <- x+ z

ls()
rm(a,b)
ls()

rm(x,y,z)
ls()

x <-c(1,2,3,4)
rm(x)
# Creating matrics and indexing

x <- matrix (data =c(1,2,3,4),nrow=2, ncol=2)
x1 <- matrix (c(1,2,3,4),2,2)

rm(x, x1)

# Generate two vectors with random variable
 set.seed(42)
 x <- rnorm(50)
y<- x+rnorm(50, mean=50, sd=0.1)

#Scatterplot of two vectors
plot(x,y)
plot(x,y, xlab="Advertising budget",ylab="Sales", main="Simulated data" )

#Co-relation
cor(x,y)

#Adding a regression line
abline(lm(y~x), col="red")

#Read a data file
auto <- read.table("http://www-bcf.usc.edu/~gareth/ISL/Auto.data")
dim(auto)
head(auto)
View(auto)

auto <- read.table("http://www-bcf.usc.edu/~gareth/ISL/Auto.data", header=TRUE, na.strings="?")
View(auto)

#check the column headers
names(auto)

#Remove missing vales

auto <- na.omit(auto)

#Indexing a data frame

auto$year
table(auto$year)

install.packages("swirl")
