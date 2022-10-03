# Purpose: To look at classification trees


# Load in library ---------------------------------------------------------

library(tree)
library(ISLR2)
library(tidyverse)
attach(Carseats)


# Classification Trees ----------------------------------------------------

high <- factor(ifelse(Sales <= 8, "No", "Yes"))
Carseats <- as_tibble(data.frame(Carseats, high))

CarseatsUpdated <- Carseats[ , -1]

treeCarseats <- tree(high ~ ., CarseatsUpdated)
summary(treeCarseats)

plot(treeCarseats)
text(treeCarseats, pretty = 1)


# Test Error for Model ----------------------------------------------------

set.seed(2)
n = nrow(CarseatsUpdated)
trainprop = 0.5
train <- sample(1:n, round(n * trainprop))
data.test <- CarseatsUpdated[-train, ]
high.test <- high[-train]

treeTestCarseats <- tree(high ~ ., CarseatsUpdated, subset = train)
tree.pred <- predict(treeTestCarseats, data.test, type = "class")

table(tree.pred, high.test)
sum(table(tree.pred, high.test))

(13 + 33) / 200 # This is the true error of the model


# Prune the tree ----------------------------------------------------------

set.seed(7)
cv.carseats <- cv.tree(treeTestCarseats, FUN = prune.misclass)
cv.carseats

bestSize <- cv.carseats$size[order(cv.carseats$dev)[1]]
prune.carseats <- prune.misclass(treeTestCarseats, best = bestSize)

plot(prune.carseats)
text(prune.carseats, pretty = 0)

prune.pred <- predict(prune.carseats, data.test, type = "class")
table(prune.pred, high.test)
sum(table(prune.pred, high.test))

(20 + 25) / 200 # Error of best model


# Regression Trees --------------------------------------------------------

n = nrow(Boston)
tree.Boston <- tree(medv ~ ., Boston)
summary(tree.Boston)

plot(tree.Boston)
text(tree.Boston, pretty = 1)
