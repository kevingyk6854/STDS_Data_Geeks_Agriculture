# load packages, clear environment ----
library(tidyverse)
library(here)
library(janitor)
library(dplyr)
library(ggplot2)
library(caret)
library(hydroGOF)
library(MASS)
library(GGally)
library(ggfortify)

remove(list = ls())

# read data ----
yield_by_region <- readRDS(here("project/src/output/yield_by_region.rds"))

# view data summaries ----
glimpse(yield_by_region)
summary(yield_by_region)
str(yield_by_region)

# set train and test ----
train_size <- floor(0.7 * nrow(yield_by_region))

set.seed(10) 
train_n <- sample(seq_len(nrow(yield_by_region)), size = train_size)

train <- yield_by_region[train_n, ]
test <- yield_by_region[-train_n, ]

# check sizes
nrow(yield_by_region) == nrow(train) + nrow(test)


# Histogram over response variable
ggplot(yield_by_region, aes(x=yield)) + 
  geom_histogram(binwidth=2)


# select features ----
train_feat <- train %>% 
  dplyr::select(lon:yield, total_water_used:source_other)


# first shot

fit_first <- glm(yield ~ .,
                  
                  family = gaussian(link = "identity"),
                  
                  data = train_feat)


summary(fit_first)

# Select subset with AIC 

step <- stepAIC(fit_first, direction="both")

step


# Ploting 

var_2 <- c(
           # 'lu_min_temp_annual', 
           # 'lu_min_temp_summer', 
           # 'lu_min_temp_winter', 
           # 'lu_mean_temp_annual', 
           # 'lu_mean_temp_nov', 
           # 'lu_mean_temp_dec', 
           # 'lu_mean_temp_jun', 
           # 'lu_mean_temp_sep', 
           # 'lu_max_temp_winter', 
           # 'lu_mean_solar_annual', 
           # 'lu_mean_solar_dry', 
           # 'lu_mean_solar_wet', 
           'lu_mean_rain_annual', 
           'lu_mean_rain_wet', 
           'lu_mean_rain_oct', 
           'lu_mean_rain_dec', 
           'lu_mean_rain_jan', 
           'lu_mean_rain_apr', 
           'lu_mean_rain_aug', 
           'source_dams_tanks', 
           'lu_carbon', 
           'lu_nitrogen', 
           'lu_ph'
           )

df_2 <- train_feat %>% 
  dplyr::select(yield, var_2)
    

df_2 %>% ggpairs()






# Second fit

fit_2 <- glm(yield ~ 
               
               lu_min_temp_annual +
               lu_min_temp_summer + 
               lu_min_temp_winter +
               
               lu_mean_temp_annual +  
               lu_mean_temp_nov +
               lu_mean_temp_dec +
               lu_mean_temp_jun +
               lu_mean_temp_sep +
               
               lu_max_temp_winter +  
               
               lu_mean_solar_annual + 
               lu_mean_solar_dry +
               lu_mean_solar_wet +
               
               poly(lu_mean_rain_annual, 2) +
               poly(lu_mean_rain_wet, 2) +
               poly(lu_mean_rain_oct, 2) +  
               poly(lu_mean_rain_dec, 2) +
               poly(lu_mean_rain_jan, 2) +
               poly(lu_mean_rain_apr, 2) +   
               poly(lu_mean_rain_aug, 2) + 
               
               source_dams_tanks +
               
               poly(lu_carbon, 2) +
               poly(lu_nitrogen, 2) +  
               lu_ph, 
               
               
               family = gaussian(link = "identity"),
                 
               data = train_feat)


summary(fit_2)

train_feat$residuals <- residuals(fit_2) # Calculate residuals values of model for training data 

train_feat$predicted <- predict(fit_2, train_feat, type="response") # Calculate predicted values of model for training data 

rmse_train <- sqrt(mean(train_feat$residuals ** 2)) # Root Mean Squared Error for training data 

test$predicted <- predict(fit_2, test) # Calculate residuals values of model for training data 

test$residuals <- test$yield - test$predicted #  Calculate residuals values of model for test data 

rmse_test <- sqrt(mean(test$residuals ** 2)) # Root Mean Squared Error for test data 

rmse_train/rmse_test # Check RMSEs shouldnt exceed 15% of each other

autoplot(fit_2)




