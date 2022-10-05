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

set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston) / 2)
tree.boston <- tree(medv ~ ., Boston, subset = train)
summary(tree.boston)

plot(tree.boston)
text(tree.boston, pretty = 0)

# Now see if we can improve performance
cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = "b")

# Prune the tree
prune.boston <- prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston, pretty = 0)

# Make predictions on the data set
yhat <- predict(tree.boston, newdata = Boston[-train, ])
boston.test <- Boston[-train, "medv"]
plot(yhat, boston.test)
abline(0, 1)
mean((yhat - boston.test)^2) # Shows the MSE


# Bagging and Random Forests ----------------------------------------------

library(randomForest)
set.seed(1)
bag.boston <- randomForest(medv ~ ., 
                           data = Boston,
                           subset = train, 
                           mtry = 12, # Shows that all 12 predictors should be considered for each split in the tree
                           importance = TRUE)
bag.boston

yhat.bag <- predict(bag.boston, newdata = Boston[-train, ])
plot(yhat.bag, boston.test)
abline(0, 1)
mean((yhat.bag - boston.test)^2) # Shows the MSE of bagging

# Change the number of trees grown
bag.boston <- randomForest(medv ~ ., data = Boston,
                           subset = train, mtry = 12, ntree = 25)
yhat.bag <- predict(bag.boston, newdata = Boston[-train, ])
mean((yhat.bag - boston.test)^2)

# Change the mtry statement to see if this helps
set.seed(1)
rf.boston <- randomForest(medv ~ ., 
                          data = Boston,
                          subset = train,
                          mtry = 6, 
                          importance = TRUE)
yhat.rf <- predict(rf.boston, newdata = Boston[-train, ])
mean((yhat.rf - boston.test)^2) #We can see from this lower MSE that random forests have now seen improvement

importance(rf.boston) # View the importance of each variable. The first is based upon the mean decrease of accuracy in predictions on the out of bag samples when a given variable is permuted. The second is a measure of the total decrease in node impurity that results from splits over that variable, averaged over all trees

varImpPlot(rf.boston) # Plot importance


# Boosting ----------------------------------------------------------------

# Here we can boost the number of trees we want
library(gbm)
set.seed(1)
boost.boston <- gbm(medv ~ ., 
                    data = Boston[train, ],
                    distribution = "gaussian", # Say what distribution you want
                    n.trees = 5000, # The number of trees you want
                    interaction.depth = 4) # The depth of each tree

summary(boost.boston) # See the influence; also plots it

# These illustrate the partial marginal dependence each variable has
plot(boost.boston, i = "rm")
plot(boost.boston, i = "lstat")

# Use the best boosted model for prediction on median house prices 
yhat.boost <- predict(boost.boston,
                      newdata = Boston[-train, ], 
                      n.trees = 5000)
mean((yhat.boost - boston.test)^2) # MSE; this is better than bagging and random forest so we can see for this data this is the best way to predict the data

# If we want to, we can perform boosting with a different value of the shrinkage parameter *lambda*; NOTE: Default lambda is 0.001
boost.boston <- gbm(medv ~ ., 
                    data = Boston[train, ],
                    distribution = "gaussian", 
                    n.trees = 5000,
                    interaction.depth = 4, 
                    shrinkage = 0.2, 
                    verbose = F)
yhat.boost <- predict(boost.boston,
                      newdata = Boston[-train, ], 
                      n.trees = 5000)
mean((yhat.boost - boston.test)^2) # MSE; less than other lambda parameter so using this *lambda* is better
