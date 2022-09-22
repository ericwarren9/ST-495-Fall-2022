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