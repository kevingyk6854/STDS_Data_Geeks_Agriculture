# load packages, clear environment ----
library(tidyverse)
library(here)
library(janitor)
library(dplyr)
library(ggplot2)
library(caret)
library(glmnet)
library(hydroGOF)

remove(list = ls())

# read data ----
yield_by_region <- readRDS(here("project/src/output/yield_by_region.rds"))

# select observations ----
yield_by_region <- yield_by_region %>% 
  filter(state == 'NSW' | state == 'WA' | state == 'Tas' | state =='Vic' | state =='SA') %>%
  filter(yield < max(yield))

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
  dplyr::select(
    grape_area_region
    ,lu_mean_rain_jan
    ,lu_ph
    ,lu_water_capacity
    ,total_water_used
    ,petit_verdot_ratio
    ,bernet_franc_ratio
    ,chardonnay_ratio
    ,colombard_ratio
    ,muscat_blanc_ratio
    ,main_chardonnay
    ,yield
    )

test_feat <- test %>% 
  dplyr::select(
    grape_area_region
    ,lu_mean_rain_jan
    ,lu_ph
    ,lu_water_capacity
    ,total_water_used
    ,petit_verdot_ratio
    ,bernet_franc_ratio
    ,chardonnay_ratio
    ,colombard_ratio
    ,muscat_blanc_ratio
    ,main_chardonnay
    ,yield
  )

# setup trainControl object
control <- trainControl(  method = "cv",              # cross-validation
                          number = 5,                 # 5 folds 
                          savePredictions = "final",
                          allowParallel = TRUE
)

# setup modelling parameters and run ----
fit <- train(   yield ~ ., 
                data = train_feat,
                method = "glm", 
                metric = "RMSE",
                trControl = control, 
                family = 'gaussian',
                preProc = c("center","scale"),
                tuneLength = 100
                
)

print(fit)
summary(fit)

# plot variable importance
ggplot(varImp(fit))
imp <- varImp(fit)

# print model coefficients
fit[["finalModel"]][["coefficients"]]

# print tuning parameter results
# fit$bestTune

# Make predictions on train
train_pred <- predict(fit, train_feat)

# Make predictions on test
test_pred <- predict(fit, test_feat)

# calculate rmse and scatter index
rmse_test <- rmse(test_pred, test$yield)
rmse_train <- rmse(train_pred, train$yield)
SI <- rmse_test/mean(yield_by_region$yield)     # (RL) changed to use rmse_test
rmse_ratio <- rmse_train/rmse_test

rmse_test
SI
rmse_ratio
