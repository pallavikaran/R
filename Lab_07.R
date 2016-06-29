rm(list=ls())
library(ISLR)
attach(Wage)
fit <- lm(wage ~ poly(age, 4))
age.grid <- seq(from = min(age), to = max(age))
pred <- predict(fit, newdata = list(age = age.grid), se = TRUE)
se.bands <- cbind(pred$fit + 2*pred$se.fit, pred$fit - 2*pred$se.fit)
plot(age, wage, cex=.5, col="darkgray", main="Degree-4Polynomial")
lines(age.grid, pred$fit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="red", lty=3)

#Exercise 1
rm(list=ls())
plot(age, wage, cex=.5, col="darkgray", main="Degree-1 to 4Polynomial")
for(i in 1:4)
{ #print(i)
  colors <- c("red", "blue", "green", "yellow")
  fit <- lm(wage ~ poly(age, i))
  age.grid <- seq(from = min(age), to = max(age))
  pred <- predict(fit, newdata = list(age = age.grid), se = TRUE)
  se.bands <- cbind(pred$fit + 2*pred$se.fit, pred$fit - 2*pred$se.fit)
  lines(age.grid, pred$fit, lwd=2, col=colors[i])
  matlines(age.grid, se.bands, lwd=1, col=colors[i], lty=3)
}

#Polynomial Logistic Reression
fit <- glm(I(wage > 250) ~ poly(age, 4), family=binomial)
pred <- predict(fit, newdata = list(age = age.grid), se = TRUE)
pfit <- exp(pred$fit) / (1+ exp(pred$fit))
se.bands.logit <- cbind(pred$fit + 2*pred$se.fit, pred$fit - 2*pred$se.fit)
se.bands <- exp(se.bands.logit) / (1 + exp(se.bands.logit))
plot(age, I(wage>250), type="n", ylim=c(0,.2))
lines(age.grid, pfit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)
rug(jitter(age[wage<=250]), side = 1, ticksize = 0.02)
rug(jitter(age[wage>250]), side = 3, ticksize = 0.02)
plot(age, wage, cex=.5, col="darkgray", main="Degree-1 to 4Polynomial")

#Exercise 2
rm(list=ls())
plot(age, I(wage>250), type="n", ylim=c(0,.2))
for(i in 1:4)
{ #print(i)
  fit <- glm(I(wage > 250) ~ poly(age, i), family=binomial)
  age.grid <- seq(from = min(age), to = max(age))
  pred <- predict(fit, newdata = list(age = age.grid), se = TRUE)
  pfit <- exp(pred$fit) / (1+ exp(pred$fit))
  se.bands.logit <- cbind(pred$fit + 2*pred$se.fit, pred$fit - 2*pred$se.fit)
  se.bands <- exp(se.bands.logit) / (1 + exp(se.bands.logit))
  if (i==1){
  lines(age.grid, pfit, lwd=2, col="red")
  matlines(age.grid, se.bands, lwd=1, col="red", lty=3)
  }
  if (i==2){
    lines(age.grid, pfit, lwd=2, col="blue")
  matlines(age.grid, se.bands, lwd=1, col="blue", lty=3) 
  }
  
  if (i==3){
    lines(age.grid, pfit, lwd=2, col="yellow")
  matlines(age.grid, se.bands, lwd=1, col="yellow", lty=3)
  }
  
  if (i==4){
    lines(age.grid, pfit, lwd=2, col="green")
  matlines(age.grid, se.bands, lwd=1, col="green", lty=3)
  }
  rug(jitter(age[wage<=250]), side = 1, ticksize = 0.02)
  rug(jitter(age[wage>250]), side = 3, ticksize = 0.02)
}

#Step Function
fit <- lm(wage ~ cut(age, 4))

#Splines
library(Splines)
print(
  age)
knot.position <- c(25, 40, 60)
fit <- lm(wage ~ bs(age, knots=knot.position), data=Wage)
pred <- predict(fit, newdata=list(age=age.grid), se=TRUE)
plot(age, wage, cex=.5, col="darkgray")
lines(age.grid, pred$fit, lwd=2, col="red")
lines(age.grid, pred$fit + 2*pred$se, lty="dashed", col="red")
lines(age.grid, pred$fit - 2*pred$se, lty="dashed", col="blue")
abline(v = knot.position, lty="dashed")

