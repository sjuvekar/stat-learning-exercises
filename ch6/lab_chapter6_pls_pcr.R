library(ISLR)
Hitters = na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

# PCR
library(pls)
?pcr
set.seed(2)
pcr.fit = pcr(Salary~., data=Hitters, scale=T, validation="CV")
summary(pcr.fit)
# Plot validation plot
validationplot(pcr.fit, val.type="MSEP")

# PCR on training data
set.seed(1)
pcr.fit = pcr(Salary~., data=Hitters, subset=train, scale=T, validation="CV")
validationplot(pcr.fit, val.type="MSEP")
pcr.pred = predict(pcr.fit, x[test, ], ncomp=7)
mean((y.test-pcr.pred)^2)

# Finally fit pcr on ENTIRE DATA using 7 components
pcr.fit = pcr(Salary~., data=Hitters, scale=T, ncomp=7)
summary(pcr.fit)


# PLS
set.seed(1)
pls.fit = plsr(Salary~., data=Hitters, subset=train, scale=T, validation="CV")
summary(pls.fit)
validationplot(pls.fit, val.type="MSEP")
pls.pred = predict(pls.fit, x[test, ], ncomp=2)
mean((y.test - pls.pred)^2)
pls.fit = plsr(y~x, ncomp=2, scale=T)
summary(pls.fit)
