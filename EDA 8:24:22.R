# PURPOSE: To look at the US Arrest Data Set

# Load the USArrests Data in from the MASS library ------------------------

rm(list=ls())
library(MASS)
data()

data.arrest <- USArrests
str(data.arrest)
help("USArrests")


# Look at some things about the data --------------------------------------

data.arrest[1, ]
data.arrest$UrbanPop

hist(data.arrest$UrbanPop)


# Use ggplot --------------------------------------------------------------

library(tidyverse)
library(usmap)

data = statepop
full = row.names(data.arrest)
data1 = cbind(full, data.arrest)
data2 = merge(data, data1)

plot_usmap(data = data2, 
           values = "UrbanPop",
           lines = "blue") +
  scale_fill_continuous(low = "white",
                        high = "red",
                        name = "Urban Population",
                        label = scales::comma) +
  theme(legend.position = "right")

plot_usmap(data = data2, 
           values = "Murder",
           lines = "blue") +
  scale_fill_continuous(low = "white",
                        high = "red",
                        name = "Murder Rate",
                        label = scales::comma) +
  theme(legend.position = "right")


# Summary of Variables ----------------------------------------------------

summary(data.arrest$Murder)

# Using subset ------------------------------------------------------------

row.names(data.arrest)[which(data.arrest$Murder == max(data.arrest$Murder))]
subset(data.arrest, Murder > 12, select = "Murder")

arrests <- data.arrest$Murder +
  data.arrest$Assault +
  data.arrest$Rape
newdata <- cbind(data.arrest, arrests)


# Exporting Visual Output -------------------------------------------------

ggplot(iris, 
       aes(x = Sepal.Length,
           y = Sepal.Width)) +
  geom_point(alpha = .3,
             color = "blue") +
  theme_bw()

ggplot(iris, 
       aes(x = Sepal.Length,
           y = Sepal.Width,
           color = Species)) +
  geom_point(alpha = .3) +
  theme_bw()



# Make an area plot -------------------------------------------------------


library(gcookbook)
library(gridExtra)

help("uspopage")
data_pop <- uspopage

yrs = unique(data_pop$Year)
tot = rep(NA, length(yrs))
for (i in 1:length(yrs)) {
  foo = subset(data_pop, Year == yrs[i])
  tot[i] = sum(foo$Thousands)
}
data_poptot <- data.frame(yrs, tot)

# Area Graph
ggplot(data_poptot, aes(x = yrs, 
                        y = tot)) + 
  geom_area() +
  theme_bw()

ggplot(data_pop,
       aes(x = Year,
           y = Thousands,
           fill = AgeGroup)) +
  geom_area(color = "black") +
  theme_bw()

# Reverse the order of our graph
data_pop1 = data_pop
data_pop1$AgeGroup = factor(data_pop1$AgeGroup,
                            levels = rev(levels(data_pop1$AgeGroup)))
ggplot(data_pop1,
       aes(x = Year,
           y = Thousands,
           fill = AgeGroup)) +
  geom_area(color = "black") +
  theme_bw()
