library (tidyverse) 
library (dplyr)
library (lubridate)
library (expss)
library (ggplot2)
library (stringr)
library(ggthemes)
library(here)
library(leaflet)
library(htmlwidgets)
library(corrplot)
library(hydroGOF)

yield_by_region_1 <- readRDS(here::here("project/src/output", "yield_by_region.rds")) %>% 
  dplyr::select(yield ,
                # state ,
                # lon ,
                # lat ,
                lu_mean_temp_summer,
                lu_mean_temp_jan ,
                lu_mean_temp_feb ,
                lu_mean_temp_sep ,
                lu_mean_temp_oct ,
                lu_mean_temp_nov ,
                lu_mean_temp_dec ,
                lu_mean_solar_wet ,
                lu_ph ,
                lu_water_capacity ,
  )
yield_by_region_1$lu_sand=NULL  
# Change Charactor Data to Factors
yield_by_region_1=yield_by_region_1 %>% mutate_if(is.character, as.factor)

# Define Train and Testing set
train_size <- floor(0.7 * nrow(yield_by_region_1))

set.seed(10) 
train_n <- sample(seq_len(nrow(yield_by_region_1)), size = train_size)

train <- yield_by_region_1[train_n, ]
test <- yield_by_region_1[-train_n, ]

# Corr Plot
num_vars <- unlist(lapply(train, is.numeric))
train_nums <- train[ , num_vars]

train_corr <- cor(train_nums)
corrplot(train_corr, method="pie")
# Linear Model
model1=lm(yield ~.,data=train)

summary(model1)

model2=lm(yield ~.,data=test)

summary(model2)

# Predict Test Set
test$predict=predict(model1,test)


# Predict Train Set
train$predict=predict(model1,train)

rmse_train <- rmse(train$predict, train$yield)



# Predict test set
test$predict=predict(model2,test)

rmse_test <- rmse(test$predict, test$yield)

rmse_ratio <- rmse_train/rmse_test


yield_by_region_1_BK <- train
#lets add some colums to our tibble to represent the predicted values and the resulting residuals (we will need these to draw the plot)
yield_by_region_1_BK$predicted <- predict(model1,train)   # Save the predicted values
yield_by_region_1_BK$residuals <- residuals(model1) # Save the residual values


ggplot(yield_by_region_1_BK, aes(x = lu_mean_temp_summer
                                 # +
                                 # lu_mean_temp_jan +
                                 # lu_mean_temp_feb +
                                 # lu_mean_temp_sep +
                                 # lu_mean_temp_oct +
                                 # lu_mean_temp_nov +
                                 # lu_mean_temp_dec +
                                 # lu_mean_solar_wet +
                                 # lu_ph +
                                 # lu_water_capacity
                                 , y = yield)) + # the same old ggplot base function we started with
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # plot regression slope
  geom_segment(aes(xend = lu_mean_temp_summer
                   # +
                   #   lu_mean_temp_jan +
                   #   lu_mean_temp_feb +
                   #   lu_mean_temp_sep +
                   #   lu_mean_temp_oct +
                   #   lu_mean_temp_nov +
                   #   lu_mean_temp_dec +
                   #   lu_mean_solar_wet +
                   #   lu_ph +
                   #   lu_water_capacity 
                   , yend = predicted), alpha = .3) +  # alpha used to fade the lines from the residuals
  geom_point() + #plots the actual points in our datasets $(x_i,y_i)$
  geom_point(aes(y = predicted), shape = 1) + # plots the $(x_i,\hat{y_i})$ points
  geom_point(aes(x=mean(lu_mean_temp_summer
                        # +
                        #   lu_mean_temp_jan +
                        #   lu_mean_temp_feb +
                        #   lu_mean_temp_sep +
                        #   lu_mean_temp_oct +
                        #   lu_mean_temp_nov +
                        #   lu_mean_temp_dec +
                        #   lu_mean_solar_wet +
                        #   lu_ph +
                        #   lu_water_capacity
  ),y=mean(yield)), color="red") + #plotting the mean point for both high_GPA and univ_GPA
  xlab('variables') +
  ggtitle('Residuals in Model')

plot(model1)
title(xlab= '', ylab = '')