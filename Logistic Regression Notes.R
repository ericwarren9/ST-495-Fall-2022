# PURPOSE: Lecture notes on logistic regression


# Load in libraries needed ------------------------------------------------

library(ISLR)
library(MASS)
library(tidyverse)
library(gridExtra)


# Use default data --------------------------------------------------------

dataDefault <- as_tibble(Default)


# Do quick EDA ------------------------------------------------------------

p1 <- dataDefault %>%
  ggplot(aes(x = default,
             y = balance,
             fill = default)) +
  geom_boxplot() +
  theme_bw()

p2 <- dataDefault %>%
  ggplot(aes(x = default,
             y = income,
             fill = default)) +
  geom_boxplot() +
  theme_bw()

grid.arrange(p1, p2, nrow = 2, ncol = 1)

table(dataDefault$default, dataDefault$student)
round(206 / (206+6850), 3) # Find percentage of non-students who have defaulted
round(127 / (127+2817), 3) # Find percentage of students who have defaulted


# Fit the logistic regression model ---------------------------------------

fit1 <- glm(default ~ balance, dataDefault, 
           family = "binomial")
summary(fit1)

# Interpretation of coefficient of balance
exp(5.499 * 10**-3)

# Now find odds of default charge when balance increases by 1000 dollars 
exp(5.499 * 10**-3 * 1000)

fit2 <- glm(default ~ income, dataDefault,
            family = "binomial")
summary(fit2)

fit3 <- glm(default ~ student, dataDefault,
            family = "binomial")
summary(fit3)


# Multinomial logistic regression -----------------------------------------

data.iris <- as_tibble(iris)

library(nnet)
logreg <- multinom(Species ~ ., data.iris)
summary(logreg)
predict(logreg, type = "probs", newdata = data.iris)
predict(logreg, type = "class", newdata = data.iris)
logreg.pred <- predict(logreg, type = "class", newdata = data.iris)
sum(data.iris$Species != logreg.pred) / nrow(data.iris)


# Classification using LDA (linear discriminant analysis) -----------------

lda1 = lda(Species ~ Sepal.Length + Sepal.Width, data.iris)
lda1 #mean and SD
summary(lda1)
pred.lda1 <- predict(lda1, data.iris)
pred.lda1$class

# how to quantify the misclassification error?
sum(data.iris$Species != pred.lda1$class) / nrow(data.iris)

# Now let us use petal dimensions
lda2 <- lda(Species ~ Petal.Length + Petal.Width, data.iris)
summary(lda2)
pred.lda2 <- predict(lda2, data.iris)
pred.lda2$class

# how to quantify the misclassification error?
sum(data.iris$Species != pred.lda2$class) / nrow(data.iris)

# Let us use all the data
lda3 <- lda(Species ~ ., data.iris)
summary(lda3)
pred.lda3 <- predict(lda3, data.iris)
pred.lda3

# how to quantify the misclassification error?
sum(data.iris$Species != pred.lda3$class) / nrow(data.iris)


# Let us classify using QDA -----------------------------------------------

qda3 <- qda(Species ~ ., data.iris)
qda3 #mean only
pred.qda3 <- predict(qda3, data.iris)
sum(data.iris$Species != pred.qda3$class) / nrow(data.iris)


# Naive Bayes -------------------------------------------------------------

library(e1071)
nb3 <- naiveBayes(Species~ ., data.iris)
nb3 #mean and SD
summary(nb3)
pred.nb3 <- predict(nb3, data.iris)
sum(data.iris$Species != pred.nb3) / nrow(data.iris)