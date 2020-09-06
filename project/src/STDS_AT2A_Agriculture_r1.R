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


### yield data ###
# Reading csv file and skipping the first 4 rows then assigning to tibble (yield_area_raw)
# (Data from: https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/7121.02015-16?OpenDocument) - link doesn't work
# Explanation of Australian Statistical Geography Standard (ASGS) regions https://www.abs.gov.au/websitedbs/d3310114.nsf/home/australian+statistical+geography+standard+(asgs)

yield_area_raw <- read_csv(here::here("project/src/data/yield", "7121DO004_201516.csv"), skip = 4)


### load gridded climate raster data ###
mean_temp <- raster(here("project/src/data/BOM_climate/mean_temperature", "meanann.txt"))
mean_rainfall <- raster(here("project/src/data/BOM_climate/mean_rainfall", "rnozan.txt"))
annual_solar <- raster(here("project/src/data/BOM_climate/solar_exposure", "solaran.txt"))

### load gridded soil raster data ###

# get surface clay content for Perth

aoi_perth <- c(115.73, -32.55, 116.05, -32.05)
Perth_surface_clay <- get_soils_data(product = 'NAT', attribute = 'CLY',
                                     component = 'VAL', depth = 1,
                                     aoi = aoi_perth, write_out = FALSE)

# set CRS projection for the rasters
crs(mean_temp) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(mean_rainfall) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(annual_solar) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(Perth_surface_clay) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

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

# load SA2 regions for SA and WA
SA2_SA_WA <- readOGR(dsn  = here("project/src/data", "SA2_2016_SA_WA.shp"), layer = "SA2_2016_SA_WA")

# load agircultural landuses for SA and WA 
landuse_SA_WA <- readOGR(dsn  = here("project/src/data", "landuse_SA_WA.shp"), layer = "landuse_SA_WA")


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

# filter only SA and WA SA2 entries by region code
region_yield_area_SA_WA <- region_yield_area_tidy %>% 
  filter(between(code, 400000000, 599999999)) %>% 
  mutate(code = as.factor(code))

### Merge Data ### 
# extract the climate and soil raster values to a list object for each SA2 region
# long time to run 
climate.temp <- raster::extract(mean_temp, SA2_SA_WA)
climate.rainfall <- raster::extract(mean_rainfall, SA2_SA_WA)
climate.solar <- raster::extract(annual_solar, SA2_SA_WA)
soil.perth.clay <-  raster::extract(Perth_surface_clay, SA2_SA_WA)

# additional step is to group the agricultural land use polygons by type and land use, then extract climate & soil data in only these areas 


# to obtain one average value for every polygon calculate the mean values with lapply
climate.mean_temp_region <- unlist(lapply(climate.temp, FUN=mean))
climate.mean_rainfall_region <- unlist(lapply(climate.rainfall, FUN=mean))
climate.mean_solar_region <- unlist(lapply(climate.solar, FUN=mean))
soil.mean_clay_region <- unlist(lapply(soil.perth.clay, FUN=mean)) #lots of NA

# merge climate and soil values for each SA2 region with yields 
SA2_SA_WA@data <- data.frame(SA2_SA_WA@data, mean_temp = climate.mean_temp_region, mean_rainfall = climate.mean_rainfall_region, mean_solar = climate.mean_solar_region, mean_soil_clay = soil.mean_clay_region)


# merge climate projection data in the same way
region_yield_area_climate_SA_WA <- left_join(region_yield_area_SA_WA, as.data.frame(SA2_SA_WA), by = c("code" = "SA2_MAIN16"))

region_yield_area_climate_SA_WA <- region_yield_area_climate_SA_WA %>% 
  dplyr::select(code:area, mean_temp:mean_solar)
