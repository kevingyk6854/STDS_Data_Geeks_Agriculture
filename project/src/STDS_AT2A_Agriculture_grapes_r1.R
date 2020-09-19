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
library(geosphere)

### loading, tidying and merging datasets ###
### yield data ###
# Reading csv file and skipping the first 4 rows then assigning to tibble (yield_area_raw)
# (Data from: https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/7121.02015-16?OpenDocument) - link doesn't work
# Explanation of Australian Statistical Geography Standard (ASGS) regions https://www.abs.gov.au/websitedbs/d3310114.nsf/home/australian+statistical+geography+standard+(asgs)

yield_area_raw <- read_csv(here::here("project/src/data/yield", "7121DO004_201516.csv"), skip = 4)


### load gridded climate raster data ###
mean_temp <- raster(here("project/src/data/BOM_climate/mean_temperature", "meanann.txt"))
mean_rainfall <- raster(here("project/src/data/BOM_climate/mean_rainfall", "rnozan.txt"))
annual_solar <- raster(here("project/src/data/BOM_climate/solar_exposure", "solaran.txt"))

# set CRS projection for the rasters
crs(mean_temp) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(mean_rainfall) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(annual_solar) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# load SA2 regions
SA2 <- readOGR(dsn  = here("project/src/data/SA2", "SA2_2016_AUST.shp"), layer = "SA2_2016_AUST")

# load agircultural landuses 
landuse <- readOGR(dsn  = here("project/src/data/landuse", "CLUM_Commodities_2018_v2.shp"), layer = "CLUM_Commodities_2018_v2")


### tidy data ###

# Select and filter wine grapes and Area (ha)
yield_area_1_tidy <- yield_area_raw %>% 
  dplyr::select ('Region code', 'Region label', 'Commodity description', 'Estimate')

colnames(yield_area_1_tidy) <- c("code", "region", "description", "yield")

yield_area_1_tidy <- yield_area_1_tidy %>%  
  filter(str_detect(description, "Grapes for wine")) %>% 
  filter(str_detect(description, "Yield")) %>% 
  dplyr::select(-description)

# Select and filter Wheat and Yield (t/ha) 
yield_area_2_tidy <- yield_area_raw %>% 
  dplyr::select ('Region label', 'Commodity description', 'Estimate', 'Number of agricultural businesses')

colnames(yield_area_2_tidy) <- c("region", "description", "area", "businesses")

yield_area_2_tidy <- yield_area_2_tidy %>%  
  filter(str_detect(description, "Grapes for wine")) %>% 
  filter(str_detect(description, "Area")) %>% 
  dplyr::select (-description)

# Join   
region_yield_area_tidy <- left_join(yield_area_1_tidy, yield_area_2_tidy)

# filter only SA2 entries by region code
region_yield_area_SA2 <- region_yield_area_tidy %>% 
  filter(between(code, 100000000, 999999999)) %>% 
  mutate(code = as.factor(code))

# filter only wine grapes landuse areas
landuse_grapes <- subset(landuse, Commod_dsc == "grapes wine")

# convert landuse polygons to points - faster computation
# Get polygons centroids
landuse_grapes_centroids <- as.data.frame(centroid(landuse_grapes))
colnames(landuse_grapes_centroids) <- c("lon", "lat") 
landuse_grapes_centroids <- data.frame("ID" = 1:nrow(landuse_grapes_centroids), landuse_grapes_centroids)

# remove errors
landuse_grapes_centroids <- landuse_grapes_centroids[landuse_grapes_centroids$lon != '-Inf',]

# Create SpatialPointsDataFrame object
coordinates(landuse_grapes_centroids) <- c("lon", "lat") 
proj4string(landuse_grapes_centroids) <- proj4string(landuse_grapes) # assign projection

# Get polygons attribute for each centroid point 
landuse_grapes_centroids@data <- sp::over(x = landuse_grapes_centroids, y = landuse_grapes, returnList = FALSE)

# remove NAs, landuses with no matching area - this appears to remove entries in obvisously incorrect locations
landuse_grapes_centroids <- subset(landuse_grapes_centroids, Commod_dsc == "grapes wine")

### Merge Data ### 
# join SA2 regions to landuses
landuse_grapes_SA2 <- sp::over(x = landuse_grapes_centroids, y = SA2, returnList = FALSE)
landuse_grapes_centroids@data <- data.frame(landuse_grapes_centroids, landuse_grapes_SA2)

# extract the climate and soil raster values to a list object for each SA2 region
# long time to run 
climate.temp <- raster::extract(mean_temp, SA2_SA_WA)
climate.rainfall <- raster::extract(mean_rainfall, SA2_SA_WA)
climate.solar <- raster::extract(annual_solar, SA2_SA_WA)
soil.clay <-  raster::extract(WA_surface_clay, SA2_SA_WA)

# to obtain one average value for every polygon calculate the mean values with lapply
climate.mean_temp_region <- unlist(lapply(climate.temp, FUN=mean))
climate.mean_rainfall_region <- unlist(lapply(climate.rainfall, FUN=mean))
climate.mean_solar_region <- unlist(lapply(climate.solar, FUN=mean))
soil.mean_clay_region <- unlist(lapply(soil.clay, FUN=mean)) # lots of NA because the sample soil data does not cover all of WA and SA

# merge climate and soil values for each SA2 region with yields 
SA2_SA_WA@data <- data.frame(SA2_SA_WA@data, mean_temp = climate.mean_temp_region, mean_rainfall = climate.mean_rainfall_region, mean_solar = climate.mean_solar_region, mean_soil_clay = soil.mean_clay_region)

region_yield_area_climate_soil_SA_WA <- left_join(region_yield_area_SA_WA, as.data.frame(SA2_SA_WA), by = c("code" = "SA2_MAIN16"))

region_yield_area_climate_soil_SA_WA <- region_yield_area_climate_soil_SA_WA %>% 
  dplyr::select(code:businesses, SA3_region = SA3_NAME16, mean_temp:mean_soil_clay)
