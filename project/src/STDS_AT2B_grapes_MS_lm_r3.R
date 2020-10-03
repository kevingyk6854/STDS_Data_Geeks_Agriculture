# load packages, clear environment ----
library(tidyverse)
library(here)
library(janitor)
library(dplyr)
library(ggplot2)
library(caret)
library(hydroGOF)

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

# select features ----
train_feat <- train %>% 
  dplyr::select(lon:lu_water_capacity, total_water_used:main_total_white)
# most signifcant features appear to be lu_ph (corrolated with lu_water_capacity), lu_mean_solar_dry, lu_mean_rain_wet

fit <- train(   x = train_feat,
                y = train$yield, 
                method = "glmnet", 
                metric = "RMSE",
                trControl = trainControl(method = "cv",number=5), 
                preProc = c("center","scale")
            )

gplot(varImp(fit))


print(fit)
summary(fit)

var_sort_by_imp <- varImp(fit)$importance %>% 
                   mutate(var_imp = row.names(.)) %>%
                   arrange(-Overall)

var_select <- var_sort_by_imp$var_imp[1:10]

train1 <- train %>% 
  dplyr::select(yield, var_select)

test1 <- test %>% 
  dplyr::select(yield, var_select)  

fit_select <- glm(yield ~ .,
                  
                  # family = inverse.gaussian(link = "log"),
                  family = gaussian(link = "identity"),
              
                  data = train1)

summary(fit_select) # Stats

vif(fit_select) # Retain model if vif (Variable Inflation Factors) of each feature is below 5. 

resid_panel(fit_select, plots = "all") # Diagnostic Plots

train1$residuals <- residuals(fit_select) # Calculate residuals values of model for training data 

train1$predicted <- predict(fit_select, train1, type="response") # Calculate predicted values of model for training data 

rmse_train1 <- sqrt(mean(train1$residuals ** 2)) # Root Mean Squared Error for training data 

test1$predicted <- predict(fit_select, test1) # Calculate residuals values of model for training data 

test1$residuals <- test1$yield - test1$predicted #  Calculate residuals values of model for test data 

rmse_test1 <- sqrt(mean(test1$residuals ** 2)) # Root Mean Squared Error for test data 

rmse_train1/rmse_test1 # Check RMSEs shouldnt exceed 15% of each other

var_select





