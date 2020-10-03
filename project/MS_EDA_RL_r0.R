library (tidyverse) 
library (dplyr)
library (lubridate)
library (expss)
library (ggplot2)
library (stringr)
library(ggthemes)
library(here)
library(corrplot)

remove(list = ls())

# read data ----
yield_by_region_1 <- readRDS(here("project/src/output/yield_by_region.rds"))


# yield_by_region_1 <- readRDS(here('project/src/output/yield_by_region.rds')) %>% 
#   filter(state == 'NSW' | state == 'WA' | state == 'Tas' | state =='Vic') 

# how yield, temperatures correlate with one another
yield_temp <- yield_by_region_1 %>%
  dplyr::filter(yield < max(yield)) %>%
  dplyr::select(yield, starts_with('lu_mean_temp'))
#  dplyr::select(yield, lu_mean_temp_may:lu_mean_temp_jul)
yield_temp %>%
  plot(main="Correlations Among Temperatures in May to July")
# correlation matrix
cor_temp <- cor(yield_temp)
cor_temp %>%
  corrplot(method = "number", type="upper")
# max correlation
str(cor_temp)
max(cor_temp[1, 2:length(colnames(cor_temp))])   # 0.3644882 --> lu_mean_temp_nov

# Observations
# not surprisings, the temperature of one month has high correlations with that of the next,
# with the strength decreasing over months
# Implications
# the temperature data should only be included in the model as one variable, since all temperature
# variables are correlated, causing multicollinearity problem to the model

# how yield, temperatures correlate with one another
yield_by_region_1 %>%
  dplyr::filter(yield < max(yield)) %>%
  dplyr::select(yield, starts_with('lu_mean_temp')) %>%
  plot()
# Observations
# There is one outlier which is significant, which belongs to state 'WA'

max(yield_by_region_1$yield)# 39.38
max(yield_by_region_1[yield_by_region_1$yield < 39.38, 'yield']) # 24.6

#### RAIN ####
# how yield, rain correlate with one another
yield_rain <- yield_by_region_1 %>%
  dplyr::filter(yield < max(yield)) %>%
  dplyr::select(yield, starts_with('lu_mean_rain'))
#  dplyr::select(yield, lu_mean_rain_may:lu_mean_rain_jul)
yield_rain %>%
  plot(
    main="Correlations Among Rain in May to July"
  )
# correlation matrix
cor_rain <- cor(yield_rain)
cor_rain %>%
  corrplot(method = "number", type="upper")
# max correlation
str(cor_rain)
max(abs(cor_rain[1, 2:length(colnames(cor_rain))]))   # 0.4789092 --> lu_mean_rain_anual
# Observations
# Excluding the one outlier which is significant, which belongs to state 'WA',
# the most correlated variable belongs to lu_mean_rain_annual

#### SOLAR ####
# how yield, solar correlate with one another
yield_solar <- yield_by_region_1 %>%
  dplyr::filter(yield < max(yield)) %>%
  dplyr::select(yield, starts_with('lu_mean_solar'))
yield_solar %>%
  plot(main="Correlations Among Solar Annual, Dry and Wet Seasons")
# correlation matrix
cor_solar <- cor(yield_solar)
cor_solar %>%
  corrplot(method = "number", type="upper")
# max correlation
str(cor_solar)
max(abs(cor_solar[1, 2:length(colnames(cor_solar))]))   # 0.3315626 --> lu_mean_solar_wet
# Observations
# Excluding the one outlier which is significant, which belongs to state 'WA',
# the most correlated variable belongs to lu_mean_rain_annual

#### SOIL ####
# how yield, solar correlate with one another
yield_soil <- yield_by_region_1 %>%
  dplyr::filter(yield < max(yield)) %>%
  dplyr::select(yield
                 , lu_bulk_density
                 , lu_carbon     # correlated with lu_water_capacity
                # , lu_clay       # correlated with lu_sand
                 , lu_nitrogen   # correlated with lu_water_capacity
                , lu_ph          # correlated with lu_water_capacity
                #, lu_phosphorus
                # , lu_sand
                # , lu_silt       # correlated with lu_sand
                , lu_water_capacity
                #,total_water_used
                )
yield_soil %>%
  plot(main="Correlations Among Soil Variables")
# correlation matrix
cor_soil <- cor(yield_soil)
cor_soil %>%
  corrplot(method = "number", type="upper")
# max correlation
str(cor_soil)
max(abs(cor_soil[1, 2:length(colnames(cor_soil))]))   # 0.6353915 --> lu_water_capacity
# Observations
# Excluding the one outlier which is significant, which belongs to state 'WA',
# lu_ph and lu_water_capacity are highly correlated
# yield correlated most with lu_water_capacity
# lu_water_capacity is highly correlated with lu_carbon and lu_nitrogen

#### COMBINED ####
yield_final <- yield_by_region_1 %>%
  dplyr::filter(yield < max(yield)) %>%
  dplyr::select(yield
                ,lu_mean_temp_nov
                ,lu_mean_rain_annual
                ,lu_mean_solar_wet
                , lu_bulk_density
                # , lu_carbon     # correlated with lu_water_capacity
                # , lu_clay       # correlated with lu_sand
                # , lu_nitrogen   # correlated with lu_water_capacity
                # , lu_ph          # correlated with lu_water_capacity
                , lu_phosphorus
                , lu_sand
                # , lu_silt       # correlated with lu_sand
                , lu_water_capacity
                ,total_water_used
  )
yield_final %>%
  plot()
# correlation matrix
cor_soil <- cor(yield_soil)
cor_soil %>%
  corrplot(method = "number", type="upper")

  
  ggplot(yield_by_region_1) +
  geom_boxplot(mapping = aes(x = state, y = yield, fill=state), show.legend = FALSE) +
  xlab("") +
  ylab("Yield in [t/ha]") +
  ggtitle("Yield for Grapes in States") +
  theme_economist()


yield_plot <- yield_by_region_1 %>%
  mutate(lu_mean_temp_nov_sqrt = sqrt(lu_mean_temp_nov)
         ,lu_mean_temp_nov_log = log10(lu_mean_temp_nov)
         )

ggplot(yield_plot) +
  geom_point(aes(x = lu_mean_temp_nov_log, y = yield, fill=lu_mean_temp_nov_log), show.legend = FALSE) +
  stat_smooth(aes(x = lu_mean_temp_nov_log, y = yield), method = "lm", col = "red") +
  xlab("") +
  ylab("Temperature") +
  ggtitle("Mean Temperature in Summer") +
  theme_economist()


ggplot(yield_by_region_1) +
  geom_boxplot(mapping = aes(x = state, y = lu_mean_rain_dry, fill=state), show.legend = FALSE) +
  xlab("") +
  ylab("Rainfall in mm") +
  ggtitle("Mean Rainfall in Dry Season") +
  theme_economist()




ggplot(yield_by_region_1) +
  geom_point(mapping = aes(x = lu_mean_temp_summer, y = yield, color = state), show.legend = FALSE) +
  geom_smooth(method = lm, aes(x = lu_mean_temp_summer, y = yield), size = 0.5, se = FALSE) +
  facet_wrap(~ state, nrow = 2) +
  xlab("") +
  ylab("Temperature") +
  ggtitle("Mean Temperature in Summer") +
  theme_economist()


ggplot(yield_by_region_1) +
  geom_point(mapping = aes(x = lu_silt, y = yield, color = state), show.legend = FALSE) +
  geom_smooth(method = lm, aes(x = lu_silt, y = yield), size = 0.5, se = FALSE) +
  facet_wrap(~ state, nrow = 2) +
  xlab("Silt [%]") +
  ylab("Yield [hl/ha]") +
  ggtitle("Silt") +
  theme_economist()

ggplot(yield_by_region_1) +
  geom_point(mapping = aes(x = lu_ph, y = yield, color = state), show.legend = FALSE) +
  geom_smooth(method = lm, aes(x = lu_ph, y = yield), size = 0.5, se = FALSE) +
  facet_wrap(~ state, nrow = 2) +
  xlab("PH [per]") +
  ylab("Yield [hl/ha]") +
  ggtitle("PH") +
  theme_economist()


