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
  dplyr::select(lu_mean_temp_annual:lu_water_capacity)
# most signifcant features appear to be lu_ph (corrolated with lu_water_capacity), lu_mean_solar_dry, lu_mean_rain_wet

# setup trainControl object
control <- trainControl(  method = "cv",              # cross-validation
                          number = 5,                 # 5 folds 
                          savePredictions = "final",
                          allowParallel = TRUE
)

# setup modelling parameters and run ----
fit <- train(   x = train_feat,
                y = train$yield, 
                method = "lm", 
                metric = "RMSE",
                trControl = trainControl(method = "cv",number=5), 
                preProc = c("center","scale")
)

print(fit)
summary(fit)
plot(fit)

# predict on our test set ----
test_feat <- test %>% 
  dplyr::select(lu_mean_temp_annual:lu_water_capacity)

pred <- predict(fit, test_feat)

# calculate rmse and scatter index
rmse <- rmse(pred, test$yield)
SI <- rmse/mean(yield_by_region$yield)
