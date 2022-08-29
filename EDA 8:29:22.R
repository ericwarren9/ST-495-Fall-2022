# PURPOSE: Use the bicycle crash data to examine how to clean data before using it for analysis


# Read in the needed materials --------------------------------------------

library(tidyverse)
bicycleCrash <- read.csv("nc_bike_crash.csv", stringsAsFactors = T)[ , -c(1:2)]


# Look at AmbulanceR feature ----------------------------------------------

table(bicycleCrash$AmbulanceR)


# Change names ------------------------------------------------------------

names(bicycleCrash)[1:2] <- c("Ambulance_Required", "BikerAge_Group")


# Frequency Table ---------------------------------------------------------

table(bicycleCrash$BikerAge_Group)


# Correct issues ----------------------------------------------------------

levels(bicycleCrash$BikerAge_Group)

levels(bicycleCrash$BikerAge_Group)[3] = "6-10"
levels(bicycleCrash$BikerAge_Group)[4] = "11-15"
levels(bicycleCrash$BikerAge_Group)[1] = "NA"

table(bicycleCrash$BikerAge_Group)


# Extract Location of crashes ---------------------------------------------

a = bicycleCrash$Location
a = as.character(a)
a = strsplit(a, ",")
a = unlist(a)
loc = matrix(as.numeric(a), 
             nrow = nrow(bicycleCrash),
             byrow = T)
loc = data.frame(loc)
names(loc) = c("latitude", "longitude")

# Make visual of all the crashes
bicycleCrash <- cbind(bicycleCrash, loc) %>%
  filter(latitude != 0,
         longitude != 0)

bicycleCrash %>%
  ggplot(aes(x = longitude,
             y = latitude,
             color = Rural_Urba)) +
  geom_point(alpha = 0.3) +
  theme_bw()


# Create a numerical summary ----------------------------------------------

table(bicycleCrash$City)
length(unique(bicycleCrash$City))

cityTable <- table(bicycleCrash$City)
cityDF <- as.data.frame(cityTable) %>%
  arrange(desc(Freq))
names(cityDF)[1] <- "City"

#Find top 5 cities for crashes
cityDF[2:6, ]
