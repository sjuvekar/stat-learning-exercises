library(ISLR)
set.seed(1)

# Perform logistic regression
glm.fit = glm(default~income+balance, data=Default, family=binomial)

# Validation1
train.size = dim(Default)[1] / 2
misclassify.error = rep(0, 3)
for (i in 0:3) {
  train = sample(train.size * 2, train.size)
  glm.fit = glm(default~income+balance, data=Default, subset=train, family=binomial)
  glm.prob = predict(glm.fit, Default[-train, ], type="response")
  glm.pred = ifelse(glm.prob > 0.5, "Yes", "No")
  misclassify.error[i] = mean(glm.pred != Default[-train, ])
}

misclassify.error
plot(misclassify.error, type="l")

# Validation with student
misclassify.error.student = rep(0, 3)
for (i in 0:3) {
  train = sample(train.size * 2, train.size)
  glm.fit = glm(default~income+balance+student, data=Default, subset=train, family=binomial)
  glm.prob = predict(glm.fit, Default[-train, ], type="response")
  glm.pred = ifelse(glm.prob > 0.5, "Yes", "No")
  misclassify.error.student[i] = mean(glm.pred != Default[-train, ])
}

misclassify.error.student








