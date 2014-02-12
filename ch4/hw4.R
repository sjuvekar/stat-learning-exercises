library(ISLR)

?Weekly
attach(Weekly)
pairs(Weekly, col=Direction)

# Logistic regression
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Weekly, family=binomial)
summary(glm.fit)
glm.prob = predict(glm.fit, Weekly, type="response")
glm.pred = ifelse(glm.prob > 0.5, "Up", "Down")
table(glm.pred, Direction)
mean(glm.pred == Direction)

# train-test split
train.logical = Year < 2009
?cbind
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Weekly, subset=train.logical, family=binomial)
summary(glm.fit)
test.data = Weekly[!train.logical, ]
test.Direction = Direction[!train.logical]
glm.prob = predict(glm.fit, test.data, type="response")
glm.pred = ifelse(glm.prob > 0.5, "Up", "Down")
table(glm.pred, test.Direction)
mean(glm.pred == test.Direction)

# LDA
library(MASS)
lda.fit = lda(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Weekly, subset=train.logical)
lda.fit
plot(lda.fit)
lda.pred = predict(lda.fit, test.data)
table(lda.pred$class, test.Direction)
mean(lda.pred$class == test.Direction)

# QDA
qda.fit = qda(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Weekly, subset=train.logical)
qda.fit
qda.pred = predict(qda.fit, test.data)
table(qda.pred$class, test.Direction)
mean(qda.pred$class == test.Direction)

# KNN
library(class)
train.X = cbind(Lag1,Lag2,Lag3,Lag4,Lag5,Volume)[train.logical, ]
test.X = cbind(Lag1,Lag2,Lag3,Lag4,Lag5,Volume)[!train.logical, ]
train.Direction = Direction[train.logical]
knn.fit = knn(train.X, test.X, train.Direction, 1)
table(knn.fit, test.Direction)
mean(knn.fit == test.Direction)