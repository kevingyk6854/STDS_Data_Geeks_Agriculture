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
library(mapview)

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
proj4string(SA2) <- proj4string(mean_temp) 
  
# load agircultural landuses 
landuse <- readOGR(dsn  = here("project/src/data/landuse", "CLUM_Commodities_2018_v2.shp"), layer = "CLUM_Commodities_2018_v2")
proj4string(landuse) <- proj4string(mean_temp)

### tidy data ###

# Select and filter wine grapes and Area (ha)
yield_area_1_tidy <- yield_area_raw %>% 
  dplyr::select ('Region code', 'Region label', 'Commodity code', 'Estimate')

colnames(yield_area_1_tidy) <- c("code", "region", "comm_code", "yield")

yield_area_1_tidy <- yield_area_1_tidy %>%  
  filter(comm_code == "GRAPESTOTAL_YIELD_F") %>% 
  dplyr::select(-comm_code)

# Select and filter Wheat and Yield (t/ha) 
yield_area_2_tidy <- yield_area_raw %>% 
  dplyr::select ('Region code', 'Region label', 'Commodity code', 'Estimate', 'Number of agricultural businesses')

colnames(yield_area_2_tidy) <- c("code", "region", "comm_code", "area", "businesses")

yield_area_2_tidy <- yield_area_2_tidy %>%  
  filter(comm_code == "AGGRAPE_AHA_F") %>% 
  dplyr::select (-comm_code)

# Join   
region_yield_area_tidy <- left_join(yield_area_2_tidy, yield_area_1_tidy, by = "code")

# filter only SA2 entries by region code
# remove NA and 0 - no yield data
region_yield_area_SA2 <- region_yield_area_tidy %>% 
  filter(between(code, 100000000, 999999999)) %>% 
  filter(!is.na(yield)) %>%
  filter(yield != 0) %>% 
  mutate(code = as.factor(code)) %>% 
  dplyr::select (-region.y)

# filter only wine grapes landuse areas
landuse_grapes <- landuse %>% 
  subset(str_detect(Commod_dsc,"grapes")) %>% 
  subset(str_detect(Tertiary, "grapes|Grapes|vine"))

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
landuse_grapes_centroids@data <- data.frame(landuse_grapes_centroids, code = landuse_grapes_SA2$SA2_MAIN16, region = landuse_grapes_SA2$SA2_NAME16)

# extract the raster values to a vector for each landuse centroid
landuse_temps <- raster::extract(mean_temp, landuse_grapes_centroids)
landuse_rainfall <- raster::extract(mean_rainfall, landuse_grapes_centroids)
landuse_solar <- raster::extract(annual_solar, landuse_grapes_centroids)

# merge climate, landuses and yields 
landuse_climate <- data.frame(landuse_grapes_centroids@data, landuse_temps, landuse_rainfall, landuse_solar)

landuse_climate_yield <- left_join(landuse_climate, region_yield_area_SA2)

# tidy table
yield_by_landuse <- landuse_climate_yield %>% 
  dplyr::select(-Broad_type, -Source_yr, -Lucodev8n, -Tertiary, -Shape_Leng, -Shape_Area, -optional, -region.x, annual_temp = landuse_temps, annual_rain = landuse_rainfall, annual_solar = landuse_solar) %>% 
  filter(!is.na(yield))

# summarise for results by region
yield_by_region <- yield_by_landuse %>% 
  group_by(code) %>% 
  summarise(region = first(region), state = first(State), grape_area_region = mean(area), grape_area_landuse = sum(Area_ha), annual_temp = mean(annual_temp), annual_rain = mean(annual_rain), annual_solar = mean(annual_solar), yield = mean(yield)) 

# output tables  
write.table(yield_by_landuse, file = here("project/src/output/yield_by_landuse.csv"), sep = ",", col.names = TRUE, row.names = FALSE, append = F, quote = FALSE)    
write.table(yield_by_region, file = here("project/src/output/yield_by_region.csv"), sep = ",", col.names = TRUE, row.names = FALSE, append = F, quote = FALSE)

ggplot(yield_by_region, aes(x = annual_rain, y = yield)) +
  geom_point()

ggplot(yield_by_region, aes(x = annual_temp, y = yield)) +
  geom_point()