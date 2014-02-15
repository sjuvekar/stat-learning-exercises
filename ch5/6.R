library(ISLR)
library(boot)
set.seed(1)

# General glm 
glm.fit = glm(default~income+balance, data=Default, family=binomial)
summary(glm.fit)

# std(income) = 4.985e-06
# std(balance) = 2.274e-04

# Using bootstrap
boot.fn = function(data, index) {
  glm.fit = glm(default~income+balance, data, family=binomial, subset=index)
  coefficients(glm.fit)[-1]
}

boot.out = boot(Default, boot.fn, R = 50)
boot.out
# std(income) = 5.158596e-06
# std(balance) = 2.213958e-04