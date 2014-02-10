################### HW3 from Trevor_hestie book ##################
# Load the data
auto = read.table("../data/Auto.data", header=T, na.strings="?")
auto = na.omit(auto)
rownames(auto) = auto[, 9]
auto = auto[, -9]
names(auto)
attach(auto)

# Simple linear regression
lm.a = lm(mpg ~ horsepower, data=auto)
summary(lm.a)
predict(lm.a, data.frame(horsepower=c(98)), interval="confidence")
predict(lm.a, data.frame(horsepower=c(98)), interval="prediction")

###
#8)(a)(i) Yes, there is a rlationship between mpg and horsepower 
# indicated by low p-value for null hypothesis beta=0
# 8)(a)(ii) The mean RSE is 4.906 and R^2 is 0.6059. The F statistics is high 599
# indicating a strong relation
# 8)(a)(iii) negative

plot(horsepower, mpg)
abline(lm.a, col="red", lwd=2)
par(mfrow=c(2, 2))
plot(lm.a)

# Multiple Regression
pairs(auto)
cor(auto)
lm.b = lm(mpg~., data=auto)
summary(lm.b)
plot(lm.b)
lm.b = lm(mpg~.-acceleration-cylinders-horsepower+weight*displacement, data=auto)
summary(lm.b)
lm.b = lm(mpg~.-acceleration-cylinders-horsepower+weight*displacement+I(weight*weight), data=auto)
summary(lm.b)

################# Carseats ##############################
library(ISLR)
names(Carseats)
attach(Carseats)
lm.10 = lm(Sales~Price+Urban+US, data=Carseats)
summary(lm.10)
contrasts(Urban)
contrasts(US)
lm.alt = lm(Sales~Price+US, data=Carseats)
summary(lm.alt)
confint(lm.alt)
plot(lm.alt)


################### ANOVA #############################
set.seed(1)
x = rnorm(100)
y = 2*x + rnorm(100)
# Simple regression
lm.simple = lm(y~x+0)
summary(lm.simple)
lm.inverse = lm(x~y+0)
summary(lm.inverse)
# t-statistics
t_stat = sum(x*y) * sqrt(99) / sqrt(sum(x*x)*sum(y*y) - sum(x*y)*sum(x*y))
t_stat

# With intercept
summary(lm(y~x))
summary(lm(x~y))

# Experiment
expt = function(noise) {
  set.seed(1)
  x = rnorm(100)
  eps = rnorm(100, sd=noise)
  y = -1 + 0.5 * x + eps
  lm.exp = lm(y~x)
  print (summary(lm.exp))
  print (confint(lm.exp))
  plot(x, y, pch=20)
  abline(lm.exp, lwd=2, col="red")
  abline(-1, 0.5, col="blue")
  lm.poly = lm(y~x+I(x^2))
  print (summary(lm.poly))
  points(x, fitted(lm.poly), pch=20, col="green")  
}

expt(0.25)
expt(0.1)
expt(1.0)

# Collinearity experiment
set.seed(1)
x1 = runif(100)
x2 = 0.5*x1 + rnorm(100)/10
y = 2 + 2*x1 + 0.3*x2 + rnorm(100)
cor(x1, x2)
pairs(data.frame(x1, x2, y))
lm.coll = lm(y~x1+x2)
summary(lm.coll)
lm.single = lm(y~x1)
summary(lm.single)
lm.othersingle = lm(y~x2)
summary(lm.othersingle)
x1 = c(x1, 0.1)
x2 = c(x2, 0.8)
y = c(y, 6)
lm.mis = lm(y~x1+x2)
summary(lm.mis)
par(mfrow=c(2, 2))
plot(lm.mis)

################ Boston data #################
library(MASS)
summary(lm(crim~., data=Boston))