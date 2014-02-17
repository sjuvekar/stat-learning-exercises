library(ISLR)
names(Hitters)

dim(Hitters)
sum(is.na(Hitters$Salary))

# Omit nas
Hitters = na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

# Subset selection
library(leaps)
regfit.full = regsubsets(Salary~., data=Hitters, nvmax=19)
reg.summary = summary(regfit.full)
names(reg.summary)

reg.summary$cp
reg.summary$adjr2
reg.summary$bic
reg.summary$rsq

# Plot all these statistics
par(mfrow=c(2, 2))
plot(reg.summary$rsq, xlab="Subset size", ylab="R-squared", type="l")
plot(reg.summary$adjr2, xlab="Subset size", ylab="Adjusted R-squared", type="l", col="red")
plot(reg.summary$cp, xlab="Subset size", ylab="CP score", type="l", col="blue")
plot(reg.summary$bic, xlab="Subset size", ylab="BIC score", type="l", col="green")

# Put cross at best point
which.max (reg.summary$adjr2)
points (11, reg.summary$cp[11], col="red", pch=13)

# We call also plot the entire regfit itself
plot(regfit.full, scale="r2")
plot(regfit.full, scale="adjr2")
plot(regfit.full, scale="Cp")
plot(regfit.full, scale="bic")

# Forward and backward stepwise selection
regfit.fwd = regsubsets(Salary~., data=Hitters, nvmax=19, method="forward")
summary(regfit.fwd)
regfit.bwd = regsubsets(Salary~., data=Hitters, nvmax=19, method="backward")
summary(regfit.bwd)

# Difference from full subset selection
coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)

# Using validation to select best stepwise model
set.seed(1)
train = sample(c(T, F), nrow(Hitters), rep=T)
test = !train

# Fit on train data and find best model using validation set
regfit.train = regsubsets(Salary~., data=Hitters[train, ], nvmax=19)
test.mat = model.matrix(Salary~., data=Hitters[test, ])
val.error=rep(0, 19)
for (i in 1:19) {
  coefi = coef(regfit.train, id=i)
  predi = test.mat[, names(coefi)] %*% coefi
  val.error[i] = mean((Hitters$Salary[test] - predi) ^ 2)
}
val.error
which.min(val.error)
coef(regfit.train, id=10)

# Encapsulate entire call into a method
predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefs = coef(object, id)
  xvars = names(coefs)
  mat[, xvars] %*% coefs
}

predict.regsubsets(regfit.train, Hitters[test, ], 10)

# Once we find the optimum size of model, we perform subset selection with that size on
# ENTIRE DATA
regfit.best = regsubsets(Salary~., data=Hitters, nvmax=19)
coef(regfit.best, 10)

# k-fold cross validation to choose best model
k = 10
set.seed(1)
folds = sample(1:k, nrow(Hitters), replace=TRUE)
cv.error = matrix(NA, k, 19, dimnames=list(NULL, paste(1:19)))
for (j in 1:k) {
  best.fit = regsubsets(Salary~., data=Hitters[folds != j, ], nvmax=19)
  for (i in 1:19) {
    pred = predict.regsubsets(best.fit, Hitters[folds == j, ], i)
    cv.error[j, i] = mean((Hitters$Salary[folds == j] - pred)^2)
  }
}

mean.cv = apply(cv.error, 2, mean)
mean.cv
par(mfrow=c(1, 1))
plot(mean.cv, type="b")

reg.best = regsubsets(Salary~., data=Hitters, nvmax=19)
coef(reg.best, 11)
