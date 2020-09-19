### load libraries ###
library(here)
library(tidyverse) 
library(stringr)
library(ggplot2)
library(rgdal)
library(raster)
library(rsdmx)
library(sp)
library(slga)
library(readr)

### loading, tidying and merging datasets ###
### yield data ###
# Reading csv file and skipping the first 4 rows then assigning to tibble (yield_area_raw)
# (Data from: https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/7121.02015-16?OpenDocument) - link doesn't work
# Explanation of Australian Statistical Geography Standard (ASGS) regions https://www.abs.gov.au/websitedbs/d3310114.nsf/home/australian+statistical+geography+standard+(asgs)

yield_area_raw <- read_csv(here::here("project/src/data/yield", "7121DO004_201516.csv"), skip = 4)

### tidy data ###

# Select and filter Wheat and Area (ha)
yield_area_1_tidy <- yield_area_raw %>% 
  dplyr::select ('Region code', 'Region label', 'Commodity description', 'Estimate')

colnames(yield_area_1_tidy) <- c("code", "region", "description", "yield")

yield_area_1_tidy <- yield_area_1_tidy %>%  
  filter(str_detect(description, "Grapes - Total")) %>% 
  filter(str_detect(description, "Yield")) %>% 
  dplyr::select(-description)

# Select and filter Wheat and Yield (t/ha) 
yield_area_2_tidy <- yield_area_raw %>% 
  dplyr::select ('Region label', 'Commodity description', 'Estimate', 'Number of agricultural businesses')

colnames(yield_area_2_tidy) <- c("region", "description", "area", "businesses")

yield_area_2_tidy <- yield_area_2_tidy %>%  
  filter(str_detect(description, "Grapes - Total")) %>% 
  filter(str_detect(description, "Area")) %>% 
  dplyr::select (-description)

# Join   
region_yield_area_tidy <- left_join(yield_area_1_tidy, yield_area_2_tidy)

# filter by region code
region_yield_area_region <- region_yield_area_tidy %>% 
  filter(between(code, 1, 8)) %>% 
  mutate(code = as.factor(code))

# plot area by state
region_yield_area_region %>% 
  ggplot(aes(x = region, y = as.numeric(area))) + 
  geom_col() +
  theme(axis.text.x  = element_text(angle=90, vjust=0.3), legend.position = "none")

# load agircultural landuses
landuse <- readOGR(dsn  = here("project/src/data/landuse", "CLUM_Commodities_2018_v2.shp"), layer = "CLUM_Commodities_2018_v2")
landUse_df <- as.data.frame(landuse)

landUse_df %>% 
  filter(Commod_dsc == "grapes") %>% 
  group_by(State) %>% 
  summarise(total_area = sum(Area_ha), num_areas = n())
