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

# Importing data 

yield_region <- read_csv(here('project/src/output/yield_by_region.csv')) %>% 
  filter(state == 'NSW' | state == 'WA' | state == 'Tas' | state =='Vic' | state =='SA') 


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

boxB(yield_region$yield, k=1.5, method='asymmetric', weights=NULL, id=NULL,
     exclude=NA, logt=FALSE)

# Character, identifies the method to be used: method="resistant" provides the
# ‘standard’ boxplot fences; method="asymmetric" is a modification of standard
# method to deal with (moderately) skewed data; method="adjbox" uses Hubert
# and Vandervieren (2008) adjusted boxplot for skewed distributions.


# Simple Linear Regression

model_simple <- lm(yield ~ lu_water_capacity, data = yield_region)

summary(model_simple)

resid_panel(model_simple, plots = "all")


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

resid_panel(model, plots = "all")


# 3)
model <- glm(yield ~ lu_water_capacity + 
               lu_mean_rain_wet, 
             
             family = inverse.gaussian(link = "log"),
             
             data = yield_region)


summary(model)

# 4) All Variables 

model_all <- glm(yield ~ 
               
               lu_mean_temp_annual +
               lu_mean_temp_winter +
               lu_mean_temp_summer +
               lu_max_temp_annual +
               lu_max_temp_winter +
               lu_max_temp_summer +
               lu_min_temp_annual +
               lu_min_temp_winter +
               lu_min_temp_summer +
               lu_mean_rain_annual +
               lu_mean_rain_dry +
               lu_mean_rain_wet +
               lu_mean_solar_annual +
               lu_mean_solar_dry +
               lu_mean_solar_wet +
               lu_bulk_density +
               lu_carbon +
               lu_clay +
               lu_nitrogen +
               lu_ph +
               lu_phosphorus +
               lu_sand +
               lu_silt +
               lu_water_capacity,
             
             
             family = gaussian(link = "identity"),
             
             data = yield_region)

summary(model_all)

resid_panel(model_all, plots = "all")





