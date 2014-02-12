library(ISLR)
set.seed(1)

##### Cross Validation
train = sample(392, 196)
lm.fit = lm(mpg~horsepower, data=Auto, subset=train)
lm.predict = predict(lm.fit, Auto)
mean((Auto$mpg - lm.predict)[-train]^2)

#### Try simple Cross validation for polynomial fit
lm.fit2 = lm(mpg~poly(horsepower, 2), data=Auto, subset=train)
lm.predict2 = predict(lm.fit2, Auto)
mean((Auto$mpg - lm.predict2)[-train]^2)

##### Leave-one-out cross validation (LOOCV)
library(boot)
glm.fit = glm(mpg~horsepower, data=Auto)
cv.err = cv.glm(Auto, glm.fit)
cv.err$delta

cv.error = rep(0, 5)
for (i in 1:5) {
  glm.fit = glm(mpg~poly(horsepower, i), data=Auto)
  cv.error[i] = cv.glm(Auto, glm.fit)$delta[1]
}

##### k-fold cross validation
set.seed(17)
cv.error = rep(0, 10)
for (i in 1:10) {
  glm.fit = glm(mpg~poly(horsepower, i), data=Auto)
  cv.error[i] = cv.glm(Auto, glm.fit, K=10)$delta[1] 
}
plot(cv.error, type="l", col="red")

##### Bootstrap
alpha.fn = function(data, index) {
  vX = var(data$X[index])
  vY = var(data$Y[index])
  cXY = cov(data$X[index], data$Y[index])
  (vY - cXY) / (vX + vY - 2 * cXY)
}

set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace=T))

boot(Portfolio, alpha.fn, R=1000)

##### Bootstrap in Linear Regression
boot.fn = function(data, index) {
  lm.fit = lm(mpg~horsepower, data, subset=index)
  coef(lm.fit)
}
set.seed(1)
boot(Auto, boot.fn, R=1000)
summary(lm(mpg~horsepower, Auto))$coef

##### Bootstrap with quadratic fit
boot.q.fn = function(data, index) {
  lm.fit = lm(mpg~horsepower + I(horsepower^2), data, subset=index)
  coef(lm.fit)
}

boot(Auto, boot.q.fn, R=1000)
summary(lm(mpg~horsepower + I(horsepower^2), Auto))$coef