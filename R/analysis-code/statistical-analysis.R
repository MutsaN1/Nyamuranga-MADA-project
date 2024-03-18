###############################
# analysis script
#
#this script loads the processed, cleaned data, does a simple analysis
#and saves the results to the results folder

#load needed packages. make sure they are installed.
library(here) #for data loading/saving
library(dplyr)
library(skimr)
library(ggplot2)
library(tidyr)
library(forcats)
library(ggplot2) #for plotting
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving

#Load the data.

#Path to data. Note the use of the here() package and not absolute paths
figdata <- here::here("data","processed_data","processeddata.rds")
#load data
explorfigdata <- readRDS(figdata)
body_weights <- pivot_longer(explorfigdata, -Week, names_to = "Category", values_to = "BodyWeight")


######################################
#Data fitting/statistical analysis
######################################

############################
#### First model fit
# fit linear model using height as outcome, weight as predictor

linCat <- lm(BodyWeight ~ Category, body_weights)  

# place results from fit into a data frame with the tidy function
linCattable <- broom::tidy(linCat)

#look at fit results
print(linCattable)

# save fit results table  
table_linCat = here("results", "resulttable1.rds")
saveRDS(linCattable, file = table_linCat)

############################
#### Second model fit

# Assuming your time series data is stored in your_data with Week as the time variable and BodyWeight as the data

# Load required library
library(strucchange)

# Fit a linear regression model with treatment variables
timeseries <- lm(BodyWeight ~ Week + I(Week^2) + I(Week^3) + Category, data = body_weights)

# Summary of the model
summary(timeseries)

# place results from fit into a data frame with the tidy function
timeseriestbl <- broom::tidy(timeseries)

#look at fit results
print(timeseriestbl)

# save fit results table  
seriestable_file2 = here("results", "resulttable2.rds")
saveRDS(timeseriestbl, file = seriestable_file2)

  