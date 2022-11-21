# Purpose: Practice using deep learning models


# Import data -------------------------------------------------------------

library(ISLR2)
library(tidyverse)

hittersUpdated <- as_tibble(na.omit(Hitters))

set.seed(9)
n <- nrow(hittersUpdated)
ntest <- trunc(n / 3)
testId <- sample(1:n, ntest)


# Fit linear regression ---------------------------------------------------

lmFit <- lm(Salary ~ ., data = hittersUpdated[-testId, ])
summary(lmFit)
lpred <- predict(lmFit, hittersUpdated[testId, ])
mean(abs(lpred - hittersUpdated$Salary[testId]))


# Calibrating a deep learning model ---------------------------------------

library(torch) # Use install_torch() when installing package for first time
library(luz)
library(torchvision)
library(torchdatasets)

torch_manual_seed(9)
modnn <- nn_module(
  initialize = function(input_size){
    self$hidden <- nn_linear(input_size, 50)
    self$activation <- nn_relu()
    self$dropout <- nn_dropout(0.4)
    self$output <- nn_linear(50, 1)
  },
  forward = function(x){
    x %>%
      self$hidden() %>%
      self$activation() %>%
      self$dropout() %>%
      self$output()
  }
)

modnn <- modnn %>%
  setup(
    loss = nn_mse_loss(),
    optimizer = optim_rmsprop,
    metrics = list(luz_metric_mae())
  ) %>%
  set_hparams(input_size = ncol(hittersUpdated))
  
x <- scale(model.matrix(Salary ~ . - 1, data = hittersUpdated))
y <- hittersUpdated$Salary

fitted <- modnn %>%
  fit(
    data = list(x[-testId, ], matrix(y[-testId], ncol = 1)),
    valid_data = list(x[testId, ], matrix(y[testId], ncol = 1)), 
    epochs = 100 # How many iterations to use
  )

# Plot this out
plot(fitted) + theme_bw()

# How did it do on error
npred <- predict(fitted, x[testId, ])
mean(abs(y[testId] - as.matrix(npred))) # Gives us mean absolute error
