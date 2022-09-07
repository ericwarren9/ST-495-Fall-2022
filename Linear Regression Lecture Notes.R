# Purpose: Linear Regression concepts with synthetic data


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
