load("10.R.RData")
x.total = rbind(x, x.test)
pca = princomp(x.total, scale=T)
total.sdev = sum(pca$sdev ^ 2)
five.sdev = sum(pca$sdev[1:5]^2)
percent = five.sdev / total.sdev
percent

# First five princomp
x.new = data.frame(pca$scores[, 1:5], col.names = 1:5)
train.new = cbind(x.new[1:300, ], y)
test.new = cbind(x.new[301:1300, ], y.test)
fit.lm = lm(y ~ ., data = train.new)
pred.lm = predict(fit.lm, test.new)
mean((y.test - pred.lm)^2)

# Fitting original data
train.data = cbind(x, y)
test.data = cbind(x.test, y.test)
fit.lm = lm(y ~ ., data=train.data)
pred.lm = predict(fit.lm, test.data)
mean((y.test - pred.lm)^2)
