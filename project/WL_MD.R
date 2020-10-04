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

##testing summer and winter temp in different states -- lu_mean_temp_summer has highest correlation
##testing rain in different states --  lu_mean_rain_mar, lu_mean_rain_apr, lu_mean_rain_oct, lu_mean_rain_nov have relatively higher correlation
##testing solar in different states -- lu_mean_solar_wet has higher correlation
##testing solid attributes in different states -- lu_ph, lu_water_capacity have higher correlation 
yield_by_region <- readRDS(here('project/src/output/yield_by_region.rds')) %>% 
  filter(state %in% c("NSW","WA","Tas","SA","Vic")) 

#yield_by_region$yield=NULL
yield_by_region$code=NULL
yield_by_region$region=NULL 
yield_by_region$state=NULL
yield_by_region$lon=NULL
yield_by_region$lat=NULL
#yield_by_region$grape_area_region=NULL
yield_by_region$grape_area_landuse=NULL
yield_by_region$lu_mean_temp_annual=NULL
yield_by_region$lu_mean_temp_winter=NULL
#yield_by_region$lu_mean_temp_summer=NULL
yield_by_region$lu_max_temp_annual=NULL
yield_by_region$lu_max_temp_winter=NULL
yield_by_region$lu_max_temp_summer=NULL
yield_by_region$lu_min_temp_annual=NULL
yield_by_region$lu_min_temp_winter=NULL
yield_by_region$lu_min_temp_summer=NULL
yield_by_region$lu_mean_temp_jan=NULL
yield_by_region$lu_mean_temp_feb=NULL
yield_by_region$lu_mean_temp_mar=NULL
yield_by_region$lu_mean_temp_apr=NULL
yield_by_region$lu_mean_temp_may=NULL
yield_by_region$lu_mean_temp_jun=NULL
yield_by_region$lu_mean_temp_jul=NULL
yield_by_region$lu_mean_temp_aug=NULL
yield_by_region$lu_mean_temp_sep=NULL
yield_by_region$lu_mean_temp_oct=NULL
yield_by_region$lu_mean_temp_nov=NULL
yield_by_region$lu_mean_temp_dec=NULL
yield_by_region$lu_mean_rain_annual=NULL
yield_by_region$lu_mean_rain_dry=NULL
yield_by_region$lu_mean_rain_wet=NULL
yield_by_region$lu_mean_rain_jan=NULL
yield_by_region$lu_mean_rain_feb=NULL
#yield_by_region$lu_mean_rain_mar=NULL
#yield_by_region$lu_mean_rain_apr=NULL
yield_by_region$lu_mean_rain_may=NULL
yield_by_region$lu_mean_rain_jun=NULL
yield_by_region$lu_mean_rain_jul=NULL
yield_by_region$lu_mean_rain_aug=NULL
yield_by_region$lu_mean_rain_sep=NULL
#yield_by_region$lu_mean_rain_oct=NULL
#yield_by_region$lu_mean_rain_nov=NULL
yield_by_region$lu_mean_rain_dec=NULL
yield_by_region$lu_mean_solar_annual=NULL
yield_by_region$lu_mean_solar_dry=NULL
#yield_by_region$lu_mean_solar_wet=NULL
yield_by_region$lu_bulk_density=NULL
yield_by_region$lu_carbon=NULL
yield_by_region$lu_clay=NULL
yield_by_region$lu_nitrogen=NULL
#yield_by_region$lu_ph=NULL
yield_by_region$lu_phosphorus=NULL
yield_by_region$lu_sand=NULL
yield_by_region$lu_silt=NULL
#yield_by_region$lu_water_capacity=NULL



# Change Charactor Data to Factors
yield_by_region=yield_by_region %>% mutate_if(is.character, as.factor)

# Define Train and Testing set
train_size <- floor(0.7 * nrow(yield_by_region))

set.seed(10) 
train_n <- sample(seq_len(nrow(yield_by_region)), size = train_size)

train_set <- yield_by_region[train_n, ]
test_set <- yield_by_region[-train_n, ]

# Corr Plot
num_vars <- unlist(lapply(train_set, is.numeric))
train_nums <- train_set[ , num_vars]

train_corr <- cor(train_nums)
corrplot(train_corr, method="number")

# Linear Model
lm_model1=lm(yield ~.,data=train_set)

summary(lm_model1)

lm_model2=lm(yield~., data = test_set)

summary(lm_model2)

# Predict Train Set
train_set$predict=predict(lm_model1,train_set)

rmse_train <- rmse(train_set$predict, train_set$yield)

# Predict test set

test_set$predict=predict(lm_model2,test_set)

rmse_test <- rmse(test_set$predict, test_set$yield)

rmse_ratio <- rmse_train/rmse_test





#EDA ------------


ggplot(yield_by_region_1,aes(x=lu_mean_rain_annual,y=yield,color=state))+
  geom_point(alpha = 0.1) +
  geom_smooth(method = lm)+
  ylim(0, 50)+
  xlim(0,1500)+
  labs(title="Annual Mean Rainfall Vs yield 2015-16 By State",
       subtitle="0 - 1500 mm",
       x="lu_mean_rain_annual",
       y="yield") +
facet_grid(state ~ .)



