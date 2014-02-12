# Load the auto 
library(MASS)
auto = read.table("Auto.data", header=T)
auto = auto[, -9]
attach(auto)

# Use mpg01 varable
auto$mpg01 = ifelse(mpg > median(mpg), 1, 0)
names(auto)
attach(auto)
pairs(auto, col=mpg01)

# Split
train.logical = year < 76
train.X = auto[train.logical, -9]
test.X = auto[!train.logical, -9]
train.Y = mpg01[train.logical]
test.Y = mpg01[!train.logical]

#LDA
lda.fit = lda(mpg01~mpg+cylinders+displacement+weight+acceleration+year, data=auto, subset=train.logical)
lda.pred = predict(lda.fit, test.X)$class
table(lda.pred, test.Y)
mean(lda.pred == test.Y)

# QDA
qda.fit = qda(mpg01~mpg+displacement+weight+acceleration+year, data=auto, subset=train.logical)
qda.pred = predict(qda.fit, test.X)$class
table(qda.pred, test.Y)
mean(qda.pred == test.Y)

# Logistic regression
glm.fit = glm(mpg01~mpg+displacement+weight+acceleration+year, data=auto, subset=train.logical, family=binomial)
glm.prob = predict(glm.fit, test.X, type="response")
glm.pred = ifelse(glm.prob > 0.5, 1, 0)
table(glm.pred, test.Y)
mean(glm.pred == test.Y)

# KNN
library(class)
train.X = cbind(mpg,displacement,weight,acceleration,year)[train.logical, ]
test.X = cbind(mpg,displacement,weight,acceleration,year)[!train.logical, ]
knn.fit = knn(train.X, test.X, train.Y, 2)
table(knn.fit, test.Y)
mean(knn.fit == test.Y)