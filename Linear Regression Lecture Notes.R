# Purpose: Linear Regression concepts with synthetic data
library(tidyverse)

# Make data ---------------------------------------------------------------

set.seed(9) #So results don't change in future
n = 100
x1 = rnorm(n)
x2 = rnorm(n)
x3 = rnorm(n)
z1 = rnorm(n)
z2 = rnorm(n)


# True data generating model ----------------------------------------------

beta0 = 1
beta1 = .3
beta2 = .4
beta3 = .5

y = beta0 + beta1*x1 + beta2*x2 + beta3*z1


# Linear Regression -------------------------------------------------------

lmFit <- lm(y ~ x1)
summary(lmFit)

lmFit2 <- lm(y ~ x2)
summary(lmFit2)

lmFit3 <- lm(y ~ x3)
summary(lmFit3)

lmFit4 <- lm(y ~ x1 + x2 + z1)
summary(lmFit4)

lmFit5 <- lm(y ~ x1 + x2 + x3 + z1)
summary(lmFit5)


# Examine Linear Regression with Boston data ------------------------------

library(ISLR2)
library(MASS)
Boston <- as_tibble(Boston)

# Do simple regression
bostonFit <- lm(medv ~ lstat, Boston)
summary(bostonFit)

# Make confidence intervals
confint(bostonFit, level = 0.95)

# Also make predictions
predict(bostonFit, 
        tibble(lstat = c(5, 50, 90)),
        interval = "prediction") # Note: 50 and 90 are extrapolated

# Prediction vs confidence intervals
predict(bostonFit, 
        tibble(lstat = c(5, 10)),
        interval = "prediction")
predict(bostonFit, 
        tibble(lstat = c(5, 10)),
        interval = "confidence")

# Diagnostics
par(mfrow = c(2, 2))
plot(bostonFit)

# Make another linear regression model
bostonFit2 <- lm(medv ~ lstat + age, Boston)

# Check Correlation
cor(Boston$lstat, Boston$age)

# Non-linear transformations
bostonFit3 <- lm(medv ~ lstat + I(lstat^2), Boston)
summary(bostonFit3)

anova(bostonFit, bostonFit3)

# Finding the best model
allBostonFit <- lm(medv ~ ., Boston)
noBostonFit <- lm(medv ~ 1, Boston)

modelTesting <- stepAIC(noBostonFit, 
                        scope = list(lower = noBostonFit,
                                     upper = allBostonFit),
                        direction = "both")

bestBostonFit <- lm(formula(modelTesting), Boston)
summary(bestBostonFit)


# Cross-validation --------------------------------------------------------

set.seed(9)
trainpct = 0.7
n = nrow(Boston)
train = sample(1:n,
               round(n * trainpct))
dataTrain <- Boston[train, ]
dataTest <- Boston[-train, ]

# Calibrate the model using training data
bostonFitCV <- lm(formula(modelTesting), dataTrain)

# Compute test errors 
yhat1 = predict(bostonFitCV, newdata = dataTest)
ytest = dataTest$medv
sqrt(mean((yhat1 - ytest) ** 2))

# Look at training error for comparison
yhat2 = predict(bostonFitCV, newdata = dataTrain)
ytrain = dataTrain$medv
sqrt(mean((yhat2 - ytrain) ** 2))


# Running Diagnostics for Linear Regression -------------------------------

par(mfrow = c(2, 2))
plot(bestBostonFit)

# Leverage Scores
par(mfrow = c(1, 1))
lev = hat(model.matrix(bestBostonFit))
fitted <- predict(bestBostonFit)
plot(fitted, lev)
p = 11
n = nrow(Boston)
which(lev > 10 * p / n) # Rule of thumb: 10*p/n

# Cook's distance
cooks <- cooks.distance(bestBostonFit) # Rule of thumb: 4/n or 4/(n-p-1)
which(cooks == max(cooks))
