college = read.csv("College.csv")
fix(college)
rownames(college) = college[,1]
college = college[,-1]
summary(college)
pairs(college[, 1:10])
boxplot(college$Outstate, college$Private)

Elite = rep("No", nrow(college))
Elite[college$Top10perc > 50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)
summary(college$Elite)
plot(college$Outstate, college$Elite)

par(mfrow=c(2,2))
hist(college$Top25perc, breaks=100)
hist(college$Apps, breaks=5)
hist(college$PhD, breaks=20)
hist(college$perc.alumni, breaks=25)

Auto = read.table("Auto.data", header=T, na.strings="?")
Auto = na.omit(Auto)
rownames(Auto) = Auto[, 9]
Auto= Auto[, -9]
lapply(Auto, range)
colMeans(Auto)
sapply(Auto, sd)
newAuto = Auto[-seq(10, 85), ]
colMeans(newAuto)
sapply(newAuto, sd)
pairs(Auto)

# Boston dataset
library(MASS)
Boston
?Boston
dim(Boston)
pairs(Boston)
hist(Boston$crim)
plot(Boston$crim, Boston$medv)
dim(Boston[Boston$chas == 1, ])
summary(Boston$ptratio)
Boston[Boston$medv == min(Boston$medv), ]
summary(Boston[Boston$rm > 7, ])
summary(Boston[Boston$rm > 8, ])
