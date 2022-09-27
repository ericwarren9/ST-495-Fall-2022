# Purpose Classification (LDA, QDA, SVM) in R


# Get the required libraries ----------------------------------------------

library(MASS)
library(tidyverse)
library(gridExtra)
library(e1071)


# Import the Iris dataset -------------------------------------------------

data.iris <- as_tibble(iris)


# Cross-validation --------------------------------------------------------

K = 1000
n = nrow(data.iris)
train.prop = 0.4
train.size <- ceiling(n*train.prop)
test.size <- n-train.size

lda.err.cv <- rep(NA,K)
qda.err.cv <- rep(NA,K)
svm.err.cv <- rep(NA,K)


ptm1 <- proc.time()
for (i in 1:K){
  foo <- sample(n,train.size)
  train.data <- data.iris[foo, ]
  test.data <- data.iris[-foo, ]
  lda.cv <- lda(Species ~ ., train.data)
  qda.cv <- qda(Species ~ ., train.data)
  svm.cv <- svm(Species ~ ., train.data)
  
  pred.lda <- predict(lda.cv, test.data)
  pred.qda <- predict(qda.cv, test.data)
  pred.svm <- predict(svm.cv, test.data)
  
  lda.err.cv[i] <- sum(test.data$Species != pred.lda$class) / nrow(test.data)
  qda.err.cv[i] <- sum(test.data$Species != pred.qda$class) / nrow(test.data)
  svm.err.cv[i] <- sum(test.data$Species != pred.svm) / nrow(test.data)
}
ptm2 <- proc.time()
ptm2 - ptm1 # time taken

summary(lda.err.cv)
summary(qda.err.cv)
summary(svm.err.cv)


# Doing this in parallel --------------------------------------------------

K = 1000 
n = nrow(data.iris)
train.prop = 0.4
train.size <- ceiling(n*train.prop)
test.size <- n-train.size

library(doParallel)

ncores <- detectCores()
cl <- makeCluster(ncores-6) # note: you should use 2-3 cores for a 4-core system, 5-6 for 8 core-system, etc.
registerDoParallel(cl)

ptm1 <- proc.time()
results <- foreach(isim = 1:K, .combine = rbind, .packages = c("e1071","MASS")) %dopar%{
  foo <- sample(n,train.size)
  train.data <- data.iris[foo, ]
  test.data <- data.iris[-foo, ]
  lda.cv <- lda(Species ~ ., train.data)
  qda.cv <- qda(Species ~ ., train.data)
  svm.cv <- svm(Species ~ ., train.data)
  
  pred.lda <- predict(lda.cv, test.data)
  pred.qda <- predict(qda.cv, test.data)
  pred.svm <- predict(svm.cv, test.data)
  
  lda.err.cv[i] <- sum(test.data$Species != pred.lda$class) / nrow(test.data)
  qda.err.cv[i] <- sum(test.data$Species != pred.qda$class) / nrow(test.data)
  svm.err.cv[i] <- sum(test.data$Species != pred.svm) / nrow(test.data)
  
  c(lda.err.cv, qda.err.cv, svm.err.cv)
}
ptm2 = proc.time()
stopCluster(cl)

ptm2 - ptm1 # time taken in parallel


# Results of Classification -----------------------------------------------

lda.err.cv = results[,1]
qda.err.cv = results[,2]
svm.err.cv = results[,3]
summary(lda.err.cv)
summary(qda.err.cv)
summary(svm.err.cv)
