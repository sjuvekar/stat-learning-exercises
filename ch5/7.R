library(ISLR)

# Logistic regression of Direction
glm.fit = glm(Direction~Lag1+Lag2, data=Weekly, family=binomial)
summary(glm.fit)

# Omit first observation and fit model
glm.fit1 = glm(Direction~Lag1+Lag2, data=Weekly[-1, ], family=binomial)
summary(glm.fit1)

# Predict first observation
first.prob = predict(glm.fit1, Weekly[1, ], type="response")
first.pred = ifelse(first.prob > 0.5, "Up", "Down")
first.actual = Weekly[1, "Direction"]
first.actual == first.pred

# Perform LOOCV prediction using loop
Weekly.size = dim(Weekly)[1]
probs = rep(0, Weekly.size)
preds = rep("Up", Weekly.size)
matches = rep(0, Weekly.size)
for (i in 1:Weekly.size) {
  glm.fit = glm(Direction~Lag1+Lag2, data=Weekly[-i, ], family=binomial)
  probs[i] = predict(glm.fit, Weekly[i, ], type="response")
  if (probs[i] <= 0.5) preds[i] = "Down"
  matches[i] = ifelse(preds[i] == Weekly[i, "Direction"], 0, 1)
}

mean(matches) # 0.45

