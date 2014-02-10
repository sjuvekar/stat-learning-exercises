library(MASS)
library(ISLR)
library(car)

# Describe dataset
?Boston
names(Boston)

# Create a linear model
lm.fit = lm(medv ~ lstat, data=Boston)
lm.fit
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit)
predict(lm.fit, data.frame(lstat=c(5, 10, 15)), interval="confidence")
predict(lm.fit, data.frame(lstat=c(5, 10, 15)), interval="prediction")

# Plotting
attach(Boston)
plot(lstat, medv)
plot(lstat, medv, pch="+")
plot(lstat, medv, pch=20)
abline(lm.fit, col="red")
abline (lm.fit ,lwd =3)
abline (lm.fit ,lwd =3, col="red")
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
par(mfrow=c(2, 2))
plot(lm.fit)

par(mfrow=c(1, 1))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

# Multiple linear regression
lm.multi = lm(medv~lstat+age, data=Boston)
summary(lm.multi)
lm.multi = lm(medv~., data=Boston)
summary(lm.multi)
vif(lm.multi)

# regression with all but one variable
lm.fit1 = lm(medv~.-age, data=Boston)
summary(lm.fit1)
# Or
lm.fit1 = update(lm.multi, ~.-age)

# Interaction terms
lm.fit2 = lm(medv~lstat*age, data=Boston)
summary(lm.fit2)

# Nonlinear transformation of predictors
lm.fit3 = lm(medv~lstat + I(lstat^2), data=Boston)
summary(lm.fit3)
anova(lm.fit, lm.fit3)
par(mfrow=c(2, 2))
plot(lm.fit3)

# Ploynomial fit
lm.fit4 = lm(medv ~ poly(lstat, 5))
summary(lm.fit4)

# Log fit
lm.fit5 = lm(medv ~ log(rm), data=Boston)
summary(lm.fit5)

# Qualititative predictors
names(Carseats)
lm.fit = lm(Sales ~.+Income:Advertising+Price:Age, data=Carseats)
summary(lm.fit)