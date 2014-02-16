set.seed(1)
y = rnorm(100)
x = rnorm(100)
y = x- 2*x^2 + rnorm(100)
plot(x, y, xlab="X", ylab="Y", pch=20, col="red")

d = data.frame("x"=x, "y"=y)

# Model 1: y = b0 + b1X + e
glm.fit1 = glm(y~x, data=d)
cv1 = cv.glm(d, glm.fit1)
cv1$delta

# Model 2: y = b0 + b1X + b2X^2 + e
glm.fit2 = glm(y~x+I(x^2), data=d)
cv2 = cv.glm(d, glm.fit2)
cv2$delta

# Model 3: y = b0 + b1X + b2X^2 + + b3X^3 + e
glm.fit3 = glm(y~poly(x, 3), data=d)
cv3 = cv.glm(d, glm.fit3)
cv3$delta

# Model 4: y = b0 + b1X + b2X^2 + + b3X^3 + b4X^4 + e
glm.fit4 = glm(y~poly(x, 4), data=d)
cv4 = cv.glm(d, glm.fit4)
cv4$delta

xdata = c(1, 2, 3, 4)
ydata = c(cv1$delta[1], cv2$delta[1], cv3$delta[1], cv4$delta[1])
plot(xdata, ydata, type="l", col="blue", xlab="id", ylab="LOOCV")
