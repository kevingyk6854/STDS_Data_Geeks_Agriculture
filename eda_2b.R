
library (tidyverse) 
library (dplyr)
library (lubridate)
library (expss)
library (ggplot2)
library (stringr)
library(ggthemes)


yield_region <- readRDS(here('project/src/output/yield_by_region.rds')) %>% 
                filter(state == 'NSW' | state == 'WA' | state == 'Tas' | state =='Vic') 


ggplot(yield_region) +
  geom_boxplot(mapping = aes(x = state, y = yield, fill=state), show.legend = FALSE) +
  xlab("") +
  ylab("Yield in [t/ha]") +
  ggtitle("Yield for Grapes in States") +
  theme_economist()



ggplot(yield_region) +
  geom_boxplot(mapping = aes(x = state, y = lu_mean_temp_summer, fill=state), show.legend = FALSE) +
  xlab("") +
  ylab("Temperature") +
  ggtitle("Mean Temperature in Summer") +
  theme_economist()


ggplot(yield_region) +
  geom_boxplot(mapping = aes(x = state, y = lu_mean_rain_dry, fill=state), show.legend = FALSE) +
  xlab("") +
  ylab("Rainfall in mm") +
  ggtitle("Mean Rainfall in Dry Season") +
  theme_economist()


ggplot(yield_region) +
  geom_point(mapping = aes(x = lu_phosphorus, y = yield, color = state), show.legend = FALSE) +
  geom_smooth(method = lm, aes(x = lu_phosphorus, y = yield), size = 0.5, se = FALSE) +
  facet_wrap(~ state, nrow = 2) +
  theme_economist()

ggplot(yield_region) +
  geom_point(mapping = aes(x = lu_silt, y = yield, color = state), show.legend = FALSE) +
  geom_smooth(method = lm, aes(x = lu_silt, y = yield), size = 0.5, se = FALSE) +
  facet_wrap(~ state, nrow = 2) +
  xlab("Silt") +
  ylab("Yield") +
  ggtitle("Silt") +
  theme_economist()

ggplot(yield_region) +
  geom_point(mapping = aes(x = lu_ph, y = yield, color = state), show.legend = FALSE) +
  geom_smooth(method = lm, aes(x = lu_ph, y = yield), size = 0.5, se = FALSE) +
  facet_wrap(~ state, nrow = 2) +
  theme_economist()




ggplot(yield_region) +
  geom_point(mapping = aes(x = mean_rainfall, y = yield), se = FALSE) +
  geom_smooth(method = lm, aes(x = mean_rainfall, y = yield), size = 0.5, se = FALSE) +
  xlab("Rainfall") +
  ylab("Yield") +
  ggtitle("Plot Yield over Rainfall") +
  theme_economist()

ggplot(yield_region) +
  geom_point(mapping = aes(x = mean_solar, y = yield), se = FALSE) +
  geom_smooth(method = lm, aes(x = mean_solar, y = yield), size = 0.5, se = FALSE) +
  xlab("Solar") +
  ylab("Yield") +
  ggtitle("Plot Yield over Solar") +
  theme_economist()




