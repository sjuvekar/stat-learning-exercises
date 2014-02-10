library(ISLR)
names(Smarket)
?Smarket
dim(Smarket)
summary(Smarket)
pairs(Smarket, col=Smarket$Direction)

cor(Smarket[, -9])
plot(Smarket$Volume)

# Logistic Regression
attach(Smarket)
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial)
summary(glm.fit)
coef(glm.fit)
glm.prob = predict(glm.fit, type="response")
glm.prob[1:10]
glm.pred=ifelse(glm.prob>0.5, "Up", "Down")
glm.pred[1:10]
table(glm.pred, Direction)
mean(glm.pred == Direction)

# Find performance on test data
train = (Year < 2005)
Smarket.2005 = Smarket[!train, ]
dim(Smarket.2005)
Direction.2005 = Smarket.2005$Direction
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial, subset=train)
glm.prob = predict(glm.fit, Smarket.2005, type="response")
glm.pred = ifelse(glm.prob > 0.5, "Up", "Down")
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)

# Restricted model
glm.fit = glm(Direction~Lag1+Lag2, data=Smarket, family=binomial, subset=train)
glm.prob = predict(glm.fit, Smarket.2005, type="response")
glm.pred = ifelse(glm.prob > 0.5, "Up", "Down")
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
predict (glm.fit, data.frame(Lag1=c(1.2 ,1.5), Lag2=c(1.1, -0.8) ), type ="response")

# LDA
library(MASS)
lda.fit = lda(Direction~Lag1+Lag2, data=Smarket, subset=train)
lda.fit
plot(lda.fit)
lda.pred = predict(lda.fit, Smarket.2005)
names(lda.pred)
table(lda.pred$class, Direction.2005)
mean(lda.pred$class == Direction.2005)
sum(lda.pred$posterior[, 1] >= 0.5)
sum(lda.pred$posterior[, 1] < 0.5)

# QDA
qda.fit = qda(Direction~Lag1+Lag2, data=Smarket, subset=train)
qda.fit
qda.pred = predict(qda.fit, Smarket.2005)
names(qda.pred)
table(qda.pred$class, Direction.2005)
mean(qda.pred$class == Direction.2005)

# KNN
library(class)
?knn
train.X = cbind(Lag1, Lag2)[train, ]
test.X = cbind(Lag1, Lag2)[!train, ]
train.y = Direction[train]
set.seed(1)
knn.fit = knn(train.X, test.X, train.y, k=3)
table(knn.fit, Direction.2005)
mean(knn.fit == Direction.2005)

# Application to Caravans insurance data
dim(Caravan)
attach(Caravan)
summary(Purchase)
names(Caravan)
standardized.X = scale(Caravan[, -86])
test = 1:1000
train.X = standardized.X[-test, ]
test.X = standardized.X[test, ]
train.Y = Purchase[-test]
test.Y = Purchase[test]
set.seed(1)
knn.pred = knn(train.X, test.X, train.Y, 3)
mean(knn.pred != test.Y)
mean(test.Y != "No")
table(knn.pred, test.Y)

# Logistic regression to the Caravans insurence
glm.fit = glm(Purchase~., Caravan, family=binomial, subset=-test)
glm.prob = predict(glm.fit, Caravan[test, ], type="response")
glm.pred = ifelse(glm.prob>0.25, "Yes", "No")
table(glm.pred, test.Y)