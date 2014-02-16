library(MASS)
library(boot)

attach(Boston)

# Sample mean = estimate of population mean
mu = mean(medv)
mu
sigma = sd(medv) / sqrt(length(medv))
sigma

# mu = 22.53281, sigma =  0.4088611

# Use bootstrap
boot.fn = function(data, index) {
  mean(data[index])
}

boot.ans = boot(medv, boot.fn, R=100)
boot.ans

# mu^ = 22.53281, sigma^ = 0.3806508

# Confidence interval using bootstrap
conf.int = c(boot.ans$t0 - 2*0.3806508, boot.ans$t0 + 2*0.3806508)
conf.int

# Original confidence interval
t.test(Boston$medv)

# Median computation
mu.med = median(medv)
mu.med

# Use bootstrap for stdev
boot.fn1 = function(data, index) {
  median(data[index])
}
boot.med = boot(medv, boot.fn1, R=1000)
boot.med
# stderr = 0.3920328

# 10th percentile
q10 = quantile(medv, 0.1)
q10

boot.fn2 = function(data, index) {
  quantile(data[index], 0.1)
}
boot.q10 = boot(medv, boot.fn2, R=1000)
boot.q10
