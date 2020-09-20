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
mean_temp_annual <- raster(here("project/src/data/BOM_climate/mean_temperature", "meanann.txt"))
mean_temp_winter <- raster(here("project/src/data/BOM_climate/mean_temperature", "meanmaysep.txt"))
mean_temp_summer <- raster(here("project/src/data/BOM_climate/mean_temperature", "meanoctapr.txt"))

max_temp_annual <- raster(here("project/src/data/BOM_climate/max_temperature", "maxann.txt"))
max_temp_winter <- raster(here("project/src/data/BOM_climate/max_temperature", "maxmaysep.txt"))
max_temp_summer <- raster(here("project/src/data/BOM_climate/max_temperature", "maxoctapr.txt"))

min_temp_annual <- raster(here("project/src/data/BOM_climate/min_temperature", "minann.txt"))
min_temp_winter <- raster(here("project/src/data/BOM_climate/min_temperature", "minmaysep.txt"))
min_temp_summer <- raster(here("project/src/data/BOM_climate/min_temperature", "minoctapr.txt"))

mean_rain_annual <- raster(here("project/src/data/BOM_climate/mean_rainfall", "rnozan.txt"))
mean_rain_dry <- raster(here("project/src/data/BOM_climate/mean_rainfall", "rnozdry.txt"))
mean_rain_wet <- raster(here("project/src/data/BOM_climate/mean_rainfall", "rnozwet.txt"))

mean_solar_annual <- raster(here("project/src/data/BOM_climate/solar_exposure", "solaran.txt"))
mean_solar_dry <- raster(here("project/src/data/BOM_climate/solar_exposure", "solardry.txt"))
mean_solar_wet <- raster(here("project/src/data/BOM_climate/solar_exposure", "solarwet.txt"))

### load gridded soil raster data ###
bulk_density <- raster(here("project/src/data/soil", "bulk_density.tif"))
carbon <- raster(here("project/src/data/soil", "carbon.tif"))
clay <- raster(here("project/src/data/soil", "clay.tif"))
nitrogen <- raster(here("project/src/data/soil", "nitrogen.tif"))
ph <- raster(here("project/src/data/soil", "ph.tif"))
phosphorus <- raster(here("project/src/data/soil", "phosphorus.tif"))
sand <- raster(here("project/src/data/soil", "sand.tif"))
silt <- raster(here("project/src/data/soil", "silt.tif"))
water_capacity <- raster(here("project/src/data/soil", "water_capacity.tif"))

# set CRS projection for the rasters
crs(mean_temp_annual) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(mean_temp_winter) <-sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(mean_temp_summer) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(max_temp_annual) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(max_temp_winter) <-sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(max_temp_summer) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(min_temp_annual) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(min_temp_winter) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(min_temp_summer) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(mean_rain_annual) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(mean_rain_dry) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(mean_rain_wet) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(mean_solar_annual) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(mean_solar_dry) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(mean_solar_wet) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(bulk_density) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(carbon) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(clay) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(nitrogen) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(ph) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(phosphorus) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(sand) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(silt) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(water_capacity) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# load SA2 regions
SA2 <- readOGR(dsn  = here("project/src/data/SA2", "SA2_2016_AUST.shp"), layer = "SA2_2016_AUST")
proj4string(SA2) <- proj4string(mean_temp_annual) 
  
# load agircultural landuses 
landuse <- readOGR(dsn  = here("project/src/data/landuse", "CLUM_Commodities_2018_v2.shp"), layer = "CLUM_Commodities_2018_v2")
proj4string(landuse) <- proj4string(mean_temp_annual)

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

# filter only grapes landuse areas
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
landuse_grapes_centroids <- subset(landuse_grapes_centroids, !is.na(Commod_dsc))

### Merge Data ### 
# join SA2 regions to landuses
landuse_grapes_SA2 <- sp::over(x = landuse_grapes_centroids, y = SA2, returnList = FALSE)
landuse_grapes_centroids@data <- data.frame(landuse_grapes_centroids, code = landuse_grapes_SA2$SA2_MAIN16, region = landuse_grapes_SA2$SA2_NAME16)

# extract the raster values to a vector for each landuse centroid
lu_mean_temp_annual <- raster::extract(mean_temp_annual, landuse_grapes_centroids)
lu_mean_temp_winter <- raster::extract(mean_temp_winter, landuse_grapes_centroids)
lu_mean_temp_summer <- raster::extract(mean_temp_summer, landuse_grapes_centroids)
lu_max_temp_annual <- raster::extract(max_temp_annual, landuse_grapes_centroids)
lu_max_temp_winter <- raster::extract(max_temp_winter, landuse_grapes_centroids)
lu_max_temp_summer <- raster::extract(max_temp_summer, landuse_grapes_centroids)
lu_min_temp_annual <- raster::extract(min_temp_annual, landuse_grapes_centroids)
lu_min_temp_winter <- raster::extract(min_temp_winter, landuse_grapes_centroids)
lu_min_temp_summer <- raster::extract(min_temp_summer, landuse_grapes_centroids)
lu_mean_rain_annual <- raster::extract(mean_rain_annual, landuse_grapes_centroids)
lu_mean_rain_dry <- raster::extract(mean_rain_dry, landuse_grapes_centroids)
lu_mean_rain_wet <- raster::extract(mean_rain_wet, landuse_grapes_centroids)
lu_mean_solar_annual <- raster::extract(mean_solar_annual, landuse_grapes_centroids)
lu_mean_solar_dry <- raster::extract(mean_solar_dry, landuse_grapes_centroids)
lu_mean_solar_wet <- raster::extract(mean_solar_wet, landuse_grapes_centroids)
lu_bulk_density <- raster::extract(bulk_density, landuse_grapes_centroids)
lu_carbon <- raster::extract(carbon, landuse_grapes_centroids)
lu_clay <- raster::extract(clay, landuse_grapes_centroids)
lu_nitrogen <- raster::extract(nitrogen, landuse_grapes_centroids)
lu_ph <- raster::extract(ph, landuse_grapes_centroids)
lu_phosphorus <- raster::extract(phosphorus, landuse_grapes_centroids)
lu_sand <- raster::extract(sand, landuse_grapes_centroids)
lu_silt <- raster::extract(silt, landuse_grapes_centroids)
lu_water_capacity <- raster::extract(water_capacity, landuse_grapes_centroids)

# merge climate, landuses and yields 
landuse_climate <- data.frame(landuse_grapes_centroids@data, lu_mean_temp_annual, lu_mean_temp_winter, lu_mean_temp_summer, lu_max_temp_annual, lu_max_temp_winter, lu_max_temp_summer, lu_min_temp_annual, lu_min_temp_winter, lu_min_temp_summer, lu_mean_rain_annual, lu_mean_rain_dry, lu_mean_rain_wet, lu_mean_solar_annual, lu_mean_solar_dry, lu_mean_solar_wet, lu_bulk_density, lu_carbon, lu_clay, lu_nitrogen, lu_ph, lu_phosphorus, lu_sand, lu_silt, lu_water_capacity)

landuse_climate_yield <- left_join(landuse_climate, region_yield_area_SA2)

# tidy table
yield_by_landuse <- landuse_climate_yield %>% 
  dplyr::select(-Broad_type, -Source_yr, -Lucodev8n, -Shape_Leng, -Shape_Area, -optional, -region.x) %>% 
  filter(!is.na(yield))

# summarise for results by region
yield_by_region <- yield_by_landuse %>% 
  group_by(code) %>% 
  summarise(region = first(region), state = first(State), grape_area_region = mean(area), grape_area_landuse = sum(Area_ha), lu_mean_temp_annual = mean(lu_mean_temp_annual), lu_mean_temp_winter = mean(lu_mean_temp_winter), lu_mean_temp_summer = mean(lu_mean_temp_summer), lu_max_temp_annual = mean(lu_max_temp_annual), lu_max_temp_winter = mean(lu_max_temp_winter), lu_max_temp_summer = mean(lu_max_temp_summer), lu_min_temp_annual = mean(lu_min_temp_annual), lu_min_temp_winter = mean(lu_min_temp_winter), lu_min_temp_summer = mean(lu_min_temp_summer), lu_mean_rain_annual = mean(lu_mean_rain_annual), lu_mean_rain_dry = mean(lu_mean_rain_dry), lu_mean_rain_wet = mean(lu_mean_rain_wet), lu_mean_solar_annual = mean(lu_mean_solar_annual), lu_mean_solar_dry = mean(lu_mean_solar_dry), lu_mean_solar_wet = mean(lu_mean_solar_wet), lu_bulk_density = mean(lu_bulk_density), lu_carbon = mean(lu_carbon), lu_clay = mean(lu_clay), lu_nitrogen = mean(lu_nitrogen), lu_ph = mean(lu_ph), lu_phosphorus = mean(lu_phosphorus), lu_sand = mean(lu_sand), lu_silt = mean(lu_silt), lu_water_capacity = mean(lu_water_capacity), yield = mean(yield)) 

# output tables  
write.table(yield_by_landuse, file = here("project/src/output/yield_by_landuse.csv"), sep = ",", col.names = TRUE, row.names = FALSE, append = F, quote = FALSE)    
saveRDS(yield_by_landuse,file = here("project/src/output/yield_by_landuse.rds"))
write.table(yield_by_region, file = here("project/src/output/yield_by_region.csv"), sep = ",", col.names = TRUE, row.names = FALSE, append = F, quote = FALSE)
saveRDS(yield_by_region,file = here("project/src/output/yield_by_region.rds"))

ggplot(yield_by_region, aes(x = lu_water_capacity, y = yield)) +
  geom_point()
