# Purpose Classification (LDA, QDA, SVM) in R


# Get the required libraries ----------------------------------------------

library(MASS)
library(tidyverse)
library(gridExtra)
library(e1071)
library(class)


# Import the Iris dataset -------------------------------------------------

data.iris <- as_tibble(iris)

# Start looking at classification -----------------------------------------

n = nrow(data.iris)
train.prop = 0.4
train.size <- ceiling(n*train.prop)
test.size <- n-train.size
foo <- sample(n, train.size)
train.data <- data.iris[foo, ]
test.data <- data.iris[-foo, ]
# K-Nearest Neighbors
knn.pred <- knn(train = train.data[,1:4], test = test.data[1:4], 
                cl = train.data$Species, 
                k = 10)
table(knn.pred, test.data$Species)

# SVM classification
svm.fit <- svm(Species ~ ., data = train.data)
svm.fit
summary(svm.fit)
pred.svm <- predict(svm.fit, test.data)
table(pred.svm, test.data$Species)

# Cross-validation
K = 1000
n = nrow(data.iris)
train.prop = 0.4
train.size <- ceiling(n*train.prop)
test.size <- n-train.size


lda.err.cv <- rep(NA,K)
qda.err.cv <- rep(NA,K)
svm.err.cv <- rep(NA,K)
knn.err.cv <- rep(NA,K)


ptm1 = proc.time()
for (i in 1:K){
  foo <- sample(n,train.size)
  train.data <- data.iris[foo,]
  test.data <- data.iris[-foo,]
  
  lda.cv <- lda(Species ~ ., data = train.data)
  qda.cv <- qda(Species ~ ., data = train.data)
  svm.cv <- svm(Species ~ ., data = train.data)
  
  pred.lda <- predict(lda.cv, test.data)
  pred.qda <- predict(qda.cv, test.data)
  pred.svm <- predict(svm.cv, test.data)
  knn.pred <- knn(train = train.data[ ,1:4], 
                  test = test.data[1:4], 
                  cl = train.data$Species, 
                  k = 10)
  
  lda.err.cv[i] <- sum(test.data$Species != pred.lda$class) / nrow(test.data)
  qda.err.cv[i] <- sum(test.data$Species != pred.qda$class) / nrow(test.data)
  svm.err.cv[i] <- sum(test.data$Species != pred.svm) / nrow(test.data)
  knn.err.cv[i] <- sum(test.data$Species != knn.pred) / nrow(test.data)
}
ptm2 = proc.time()
ptm2-ptm1 # time taken

summary(lda.err.cv)
summary(qda.err.cv)
summary(svm.err.cv)
summary(knn.err.cv)

# Doing this in parallel
K = 1000
n = nrow(data.iris)
train.prop = 0.4
train.size <- ceiling(n*train.prop)
test.size <- n-train.size

library(doParallel)

ncores = detectCores()
cl = makeCluster(ncores - 6) # note: you should use 2-3 cores for a 4-core system, 5-6 for 8 core-system, etc.
registerDoParallel(cl)

ptm1 = proc.time()
results <- foreach(isim = 1:K, .combine = rbind, .packages = c("e1071","MASS","class")) %dopar%{
  foo <- sample(n,train.size)
  train.data <- data.iris[foo,]
  test.data <- data.iris[-foo,]
  
  lda.cv <- lda(Species ~ ., data = train.data)
  qda.cv <- qda(Species ~ ., data = train.data)
  svm.cv <- svm(Species ~ ., data = train.data)
  
  pred.lda <- predict(lda.cv, test.data)
  pred.qda <- predict(qda.cv, test.data)
  pred.svm <- predict(svm.cv, test.data)
  knn.pred <- knn(train = train.data[ ,1:4], 
                  test = test.data[1:4], 
                  cl = train.data$Species, 
                  k = 10)
  
  lda.err.cv[i] <- sum(test.data$Species != pred.lda$class) / nrow(test.data)
  qda.err.cv[i] <- sum(test.data$Species != pred.qda$class) / nrow(test.data)
  svm.err.cv[i] <- sum(test.data$Species != pred.svm) / nrow(test.data)
  knn.err.cv[i] <- sum(test.data$Species != knn.pred) / nrow(test.data)
  
  c(lda.err.cv, qda.err.cv, svm.err.cv, knn.err.cv)
}
ptm2 = proc.time()
stopCluster(cl)

ptm2-ptm1 # time taken in parallel

lda.err.cv = results[,1]
qda.err.cv = results[,2]
svm.err.cv = results[,3]
knn.err.cv = results[,4]

summary(lda.err.cv)
summary(qda.err.cv)
summary(svm.err.cv)
summary(knn.err.cv)
