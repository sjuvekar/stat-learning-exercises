library(ISLR)
fix(Hitters)

Hitters = na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

# Use glmnet() to fit ridge and lasso
x = model.matrix(Salary~., data=Hitters)[, -1]
y = Hitters$Salary

library(glmnet)
grid = 10 ^ seq(10, -2, length=100)
ridge.mod = glmnet(x, y, alpha=0, lambda=grid)
dim(coef(ridge.mod))
# Coefficient for lambda = 11498
ridge.mod$lambda[50]
coef(ridge.mod)[, 50]
sqrt(sum(coef(ridge.mod)[-1, 50]^2))

# Coefficients for lambda = 705
ridge.mod$lambda[60]
coef(ridge.mod)[, 60]
sqrt(sum(coef(ridge.mod)[-1, 60]^2))

# Predict
predict(ridge.mod, s=50, type="coefficients")[1:20, ]

# Use validation for predicting test MSE
set.seed(1)
train = sample(1:nrow(x), nrow(x) / 2)
test = -train
y.test = y[test]
ridge.mod = glmnet(x[train, ], y[train], alpha=0, lambda=grid, thresh=1e-12)
ridge.pred = predict(ridge.mod, newx=x[test, ], s=4)
mean((ridge.pred - y.test)^2)

# Now ridge test MSE for large lambda is same as constant model MSE
mean((mean(y[train]) - y.test)^2)
ridge.pred = predict(ridge.mod, s=1e10, newx=x[test, ])
mean((y.test - ridge.pred)^2)

# Compare with OLS where s = 0
ridge.pred = predict(ridge.mod, s=0, newx=x[test, ])
mean((y.test - ridge.pred)^2)

# Use cross-validation using cv.glmnet to select best lambda
set.seed(1)
cv.out = cv.glmnet(x[train, ], y[train], alpha=0)
plot(cv.out)
bestlambda = cv.out$lambda.min
bestlambda

ridge.pred = predict(ridge.mod, newx=x[test, ], s=bestlambda)
mean((y.test - ridge.pred)^2)

# So optimal lambda; bestlambda, is used to fit model on entire data
final.model = glmnet(x, y, alpha=0)
predict(final.model, s=bestlambda, type="coefficients")

# Lasso
lasso.mod = glmnet(x[train, ], y[train], alpha=1, lambda=grid)
plot(lasso.mod)
set.seed(1)
cv.out = cv.glmnet(x[train, ], y[train], alpha=1)
plot(cv.out)
bestlambda = cv.out$lambda.min
bestlambda
lasso.pred = predict(lasso.mod, newx=x[test, ], s=bestlambda)
mean((y.test - lasso.pred)^2)

# Not fit the entire model
final.model = glmnet(x, y, alpha=1, lambda=grid)
lasso.coef = predict(final.model, s=bestlambda, type="coefficients")[1:20, ]
lasso.coef[lasso.coef != 0]
