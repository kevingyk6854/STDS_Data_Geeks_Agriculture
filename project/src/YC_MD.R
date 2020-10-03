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
  # filter(state %in% c("NSW","WA","Tas","Vic","SA")) %>% 
  dplyr::select(yield ,
                state ,
     lon ,
    lat ,
    lu_mean_temp_summer,
# lu_mean_temp_winter,
    lu_mean_temp_jan ,
    lu_mean_temp_feb ,
   # lu_mean_temp_mar + 
    # lu_mean_temp_apr +
    # lu_mean_temp_may +
    # lu_mean_temp_jun +
    # lu_mean_temp_jul +
    # lu_mean_temp_aug +
     lu_mean_temp_sep ,
     lu_mean_temp_oct ,
   lu_mean_temp_nov ,
    lu_mean_temp_dec ,
      # lu_mean_rain_annual +
      # lu_mean_rain_jan +
      # lu_mean_rain_feb +
      # lu_mean_rain_mar +
      # lu_mean_rain_apr +
      # lu_mean_rain_may +
      # lu_mean_rain_jun +
      # lu_mean_rain_jul +
      # lu_mean_rain_aug +
      # lu_mean_rain_sep +
      # lu_mean_rain_oct +
      # lu_mean_rain_nov +
      # lu_mean_rain_dec +
    # lu_mean_solar_annual +
    # lu_mean_solar_dry +
    lu_mean_solar_wet ,
      # lu_bulk_density +
      # lu_carbon +
      # lu_clay +
      # lu_nitrogen +
      lu_ph ,
      # lu_phosphorus +
      # lu_sand +
      # lu_silt +
      # lu_clay +
      # lu_clay * lu_silt * total_water_used,
      # lu_clay * lu_silt +
     lu_water_capacity ,
      # total_water_used +
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

# Plot (How well model fit) -------------------------------------------------------------------

# Real Vs Predict Yield
ggplot(test,aes(x=predict,y=yield, color=state))+
  geom_point(alpha = 0.7)+
  #geom_abline (intercept =30,slope = 0.4)+
  labs(title="Real Vs Predict Yield", 
       x="Predict Yield",
       y="Real Yield") +
  facet_grid(state ~ .)






#EDA ------------
#quantile(yield_by_region_1$yield)
yeild_by_region=leaflet(yield_by_region_1) %>% 
  addTiles() %>% 
  addCircleMarkers(
    lng=~lon, 
    lat=~lat,
    radius = ~case_when(
      yield<=3.903~1,
      yield<=6.670~2,
      yield<=9.232~4,
      TRUE~5
    ),
    stroke = FALSE, fillOpacity = 0.9  )

# Save Map
saveWidget(widget = yeild_by_region,
           file = "yeild_by_region.html",
           selfcontained = TRUE)



