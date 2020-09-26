# Getting R ready

library (tidyverse) 
library (dplyr)
library (lubridate)
library (expss)
library (ggplot2)
library (stringr)
library (ggthemes)
library (here)
library (e1071)   
library (faraway)
library (ggResidpanel)
library (univOutl)
library (outliers)


# Importing data 

yield_region <- read_csv(here('project/src/output/yield_by_region.csv')) %>% 
  filter(state == 'NSW' | state == 'WA' | state == 'Tas' | state =='Vic' | state =='SA') 

# Spliting data in Train and Test 

yield_region$id <- 1:nrow(yield_region)   
yield_region.train <- yield_region %>% dplyr::sample_frac(.75)
yield_region.test  <- dplyr::anti_join(yield_region, yield_region.train, by = 'id')



# Ploting 

ggplot(yield_region) +
  geom_boxplot(mapping = aes(x = state, y = yield, fill=state), show.legend = FALSE) +
  xlab("") +
  ylab("Yield in [t/ha]") +
  ggtitle("Yield for Grapes in States")

ggplot(yield_region) +
  geom_point(mapping = aes(x = yield, y = lu_water_capacity, fill=state), show.legend = FALSE) +
  xlab("Yield") +
  ylab("Water Capacity") +
  ggtitle("Water Capacity") 

ggplot(yield_region) +
  geom_histogram(mapping = aes(yield), bins = 50) +
  xlab("") +
  ylab("Yield in [t/ha]") +
  ggtitle("Histogram")


# Diagnostics a bit more on EDA 

summary(yield_region)                       # stats   

skewness(yield_region$yield)                # skewness function [- left ; + right]

grubbs.test(yield_region$yield, type = 10, opposite = FALSE, two.sided = FALSE)

boxB(yield_region$yield, k=1.5, method='asymmetric', weights=NULL, id=NULL,
     exclude=NA, logt=FALSE)

# Character, identifies the method to be used: method="resistant" provides the
# ‘standard’ boxplot fences; method="asymmetric" is a modification of standard
# method to deal with (moderately) skewed data; method="adjbox" uses Hubert
# and Vandervieren (2008) adjusted boxplot for skewed distributions.


# Simple Linear Regression

fit_simple <- lm(yield ~ lu_water_capacity, data = yield_region.train)

summary(fit_simple)

resid_panel(fit_simple, plots = "all")


# Multiple Linear Regression 

model_simple <- lm(yield ~ lu_water_capacity + poly(lu_mean_rain_wet, degree = 2), data = yield_region)

summary(model_simple)

resid_panel(model_simple, plots = "all")


# Various GLM's 


# 1) 
model <- glm(yield ~ lu_ph + 
                     lu_water_capacity + 
                     lu_mean_rain_wet + 
                     lu_mean_temp_annual, 
             
             
             family = Gamma(link = "inverse"),
             
             data = yield_region)

summary(model)

resid_panel(model, plots = "all")


# 2) 
model <- glm(yield ~ lu_water_capacity + 
                     lu_mean_rain_wet, 
             
             family = gaussian(link = "identity"),
             
             data = yield_region)

summary(model)

 


# 3)
model <- glm(yield ~ lu_water_capacity + 
               lu_mean_rain_wet, 
             
             family = inverse.gaussian(link = "log"),
             
             data = yield_region)


summary(model)



# 4) All Variables 

fit_all <- glm(yield ~ 
               
               # lu_mean_temp_annual +
               # lu_mean_temp_winter +
               lu_mean_temp_summer +
               # lu_max_temp_annual +
               # lu_max_temp_winter +
               # lu_max_temp_summer +
               # lu_min_temp_annual +
               # lu_min_temp_winter +
               # lu_min_temp_summer +
               # lu_mean_rain_annual +
               # lu_mean_rain_dry +
               lu_mean_rain_wet +
               # lu_mean_solar_annual +
               # lu_mean_solar_dry +
               # lu_mean_solar_wet +
               lu_bulk_density +
               # lu_carbon +
               lu_clay +
               lu_nitrogen +
               lu_ph +
               lu_phosphorus +
               # lu_sand +
               lu_silt +
               lu_water_capacity,
             
             
             # family = inverse.gaussian(link = "log"),
             # family = inverse.gaussian(link = "1/mu^2"),
             # family = Gamma(link = "inverse"),
             family = gaussian(link = "identity"),
             
             data = yield_region.train)



summary(fit_all) # Stats

vif(fit_all) # Retain model if vif (Variable Inflation Factors) of each feature is below 5. 

resid_panel(fit_all, plots = "all") # Diagnostic Plots

yield_region.train$residuals <- residuals(fit_all) # Calculate residuals values of model for training data 

yield_region.train$predicted <- predict(fit_all, yield_region.train, type="response") # Calculate predicted values of model for training data 

rmse_train <- sqrt(mean(yield_region.train$residuals ** 2)) # Root Mean Squared Error for training data 

yield_region.test$predicted <- predict(fit_all, yield_region.test) # Calculate residuals values of model for training data 

yield_region.test$residuals <- yield_region.test$yield - yield_region.test$predicted #  Calculate residuals values of model for test data 

rmse_test <- sqrt(mean(yield_region.test$residuals ** 2)) # Root Mean Squared Error for test data 

rmse_train/rmse_test # Check RMSEs shouldnt exceed 15% of each other


