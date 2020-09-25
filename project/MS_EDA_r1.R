
library (tidyverse) 
library (dplyr)
library (lubridate)
library (expss)
library (ggplot2)
library (stringr)
library(ggthemes)
library(here)


yield_region <- readRDS(here('project/src/output/yield_by_region.rds')) %>% 
                filter(state == 'NSW' | state == 'WA' | state == 'Tas' | state =='Vic' | state =='SA') 

state_labs <- c("NSW", "SA", "TAS", "VIC", "WA")
state_labs_re <- c("VIC", "SA", "NSW", "WA", "TAS")

state_area <- yield_region %>% 
  group_by(state) %>% 
  summarise(tot_area = sum(grape_area_landuse))
  
ggplot(state_area, mapping = aes(x = reorder(state, -tot_area), y = tot_area/100, fill=state), show.legend = FALSE) +
  geom_col(show.legend = FALSE)+
  ggtitle("Grape land use by state") +
  xlab("") +
  ylab("Total area "~km^2) +
  scale_x_discrete(labels = state_labs_re)+
  theme_gdocs()
  
ggplot(yield_region) +
  geom_boxplot(mapping = aes(x = state, y = yield, fill=state), show.legend = FALSE) +
  xlab("") +
  ylab("Yield in [hl/ha]") +
  ggtitle("Grape yield by state") +
  scale_x_discrete(labels = state_labs)+
  theme_gdocs()



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
  geom_point(mapping = aes(x = lu_mean_temp_annual, y = yield, color = state), show.legend = FALSE) +
  geom_smooth(method = lm, aes(x = lu_mean_temp_annual, y = yield), size = 1, se = FALSE) +
  facet_wrap(~ state, nrow = 5) +
  xlab("Temperature (°C)") +
  ylab("Yield [hl/ha]") +
  ggtitle("Mean annual temperature") +
  theme_gdocs()+
  theme(aspect.ratio = 0.15)+
  ylim(0,30)+
  xlim(12,18)


ggplot(yield_region) +
  geom_point(mapping = aes(x = lu_mean_rain_annual, y = yield, color = state), show.legend = FALSE) +
  geom_smooth(method = lm, aes(x = lu_mean_rain_annual, y = yield), size = 1, se = FALSE) +
  facet_wrap(~ state, nrow = 5) +
  xlab("Temperature (°C)") +
  ylab("Yield [hl/ha]") +
  ggtitle("Annual rainfall") +
  theme_gdocs()+
  theme(aspect.ratio = 0.15)+
  ylim(0,30)+
  xlim(200,1250)


ggplot(yield_region) +
  geom_point(mapping = aes(x = lu_silt, y = yield, color = state), show.legend = FALSE) +
  geom_smooth(method = lm, aes(x = lu_silt, y = yield), size = 0.5, se = FALSE) +
  facet_wrap(~ state, nrow = 2) +
  xlab("Silt [%]") +
  ylab("Yield [hl/ha]") +
  ggtitle("Silt") +
  theme_economist()

ggplot(yield_region) +
  geom_point(mapping = aes(x = lu_ph, y = yield, color = state), show.legend = TRUE) +
  geom_smooth(method = lm, aes(x = lu_ph, y = yield), size = 1, se = FALSE) +
  xlab("pH") +
  ylab("Yield [hl/ha]") +
  ggtitle("Influence of soil pH")+
  scale_color_discrete(labels = state_labs)+
  labs(color = "State")+
  theme_gdocs()


