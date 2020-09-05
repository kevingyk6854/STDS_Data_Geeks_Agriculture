### load libraries ###
library(here)
library(tidyverse) 
library(stringr)
library(ggplot2)
library(rgdal)
library(raster)
library(rsdmx)

### yield data ###
# Reading csv file and skipping the first 4 rows then assigning to tibble (yield_area_raw)
# (Data from: https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/7121.02015-16?OpenDocument) - link doesn't work
# Explanation of Australian Statistical Geography Standard (ASGS) regions https://www.abs.gov.au/websitedbs/d3310114.nsf/home/australian+statistical+geography+standard+(asgs)

yield_area_raw <- read_csv(here::here("project/src/data/yield", "7121DO004_201516.csv"), skip = 4)


### load gridded climate raster data ###
mean_temp <- raster(here("project/src/data/BOM_climate/mean_temperature", "meanann.txt"))
plot(mean_temp)
mean_rainfall <- raster(here("project/src/data/BOM_climate/mean_rainfall", "rnozan.txt"))
annual_solar <- raster(here("project/src/data/BOM_climate/solar_exposure", "solaran.txt"))

# set CRS projection for the rasters
crs(mean_temp) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(mean_rainfall) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(annual_solar) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# load climate projection csv data, remove first row of units
mean_temp_2090 <- read_csv(here("project/src/data/climate_change", "mean_temp_annual_2090.csv"))[-1,]
mean_rainfall_2090 <- read_csv(here("project/src/data/climate_change", "rainfall_annual_2090.csv"))[-1,]
annual_solar_2090 <- read_csv(here("project/src/data/climate_change", "solar_radiation_annual_2090.csv"))[-1,]

# reorder columns and load as raster
mean_temp_2090 <- rasterFromXYZ(dplyr::select(mean_temp_2090, longitude, latitude, tas_annual))
mean_rainfall_2090 <- rasterFromXYZ(dplyr::select(mean_rainfall_2090, longitude, latitude, pr_annual))
annual_solar_2090 <- rasterFromXYZ(dplyr::select(annual_solar_2090, longitude, latitude, rsds_annual))

# set CRS projection for the rasters
crs(mean_temp_2090) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(mean_rainfall_2090) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(annual_solar_2090) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# additional step is to crop all raster to the same extent and then combine into a single multi-band raster with raster::stack


# Reading csv file and skipping the first 4 rows then assigning to tibble (fertilise_raw)
# (Data from: https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/4627.02016-17?OpenDocument)
fertilise_raw <- read_csv(here::here("project/src/data", "123123.csv"), skip = 4)

# load soil data


# load FAO data


### tidy data ###

# Select and filter Wheat and Area (ha)

yield_area_1_tidy <- yield_area_raw %>% 
  dplyr::select ('Region code', 'Region label', 'Commodity description', 'Estimate')

colnames(yield_area_1_tidy) <- c("code", "region", "description", "yield")

yield_area_1_tidy <- yield_area_1_tidy %>%  
  filter(str_detect(description, "Wheat")) %>% 
  filter(str_detect(description, "Yield")) %>% 
  dplyr::select(-description)

# Select and filter Wheat and Yiel (t/ha) 

yield_area_2_tidy <- yield_area_raw %>% 
  dplyr::select ('Region label', 'Commodity description', 'Estimate', 'Number of agricultural businesses')

colnames(yield_area_2_tidy) <- c("region", "description", "area", "businesses")

yield_area_2_tidy <- yield_area_2_tidy %>%  
  filter(str_detect(description, "Wheat")) %>% 
  filter(str_detect(description, "Area")) %>% 
  dplyr::select (-description)

# Join   

region_yield_area_tidy <- left_join(yield_area_1_tidy, yield_area_2_tidy)


# filter only SA2 entries by region code


### Merge Data ### 


# extract the climate raster values to a list object for each SA2 region
climate.mean_temp <- raster::extract(mean_temp, SA2)
climate.mean_rainfall <- raster::extract(mean_rainfall, SA2)
climate.annual_solar <- raster::extract(annual_solar, SA2)

# additional step is to group the agricultural land use polygons by type and land use, then extract climate data in only these areas 


# to obtain one average value for every polygon calculate the mean values with lapply
climate.mean_temp_region <- unlist(lapply(climate.mean_temp, FUN=mean))


# merge climate values for each SA2 region with yields 



