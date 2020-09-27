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
library(ggmap)
library(FNN)
library(e1071)   
library(faraway)
library(ggResidpanel)
library(univOutl)
library(outliers)
library(glmnet)
library(Matrix)
library(dplyr)
library(caret)
library(hydroGOF)
library(janitor)



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

mean_temp_jan <- raster(here("project/src/data/BOM_climate/monthly_mean_temp", "meanjan.txt"))
mean_temp_feb <- raster(here("project/src/data/BOM_climate/monthly_mean_temp", "meanfeb.txt"))
mean_temp_mar <- raster(here("project/src/data/BOM_climate/monthly_mean_temp", "meanmar.txt"))
mean_temp_apr <- raster(here("project/src/data/BOM_climate/monthly_mean_temp", "meanapr.txt"))
mean_temp_may <- raster(here("project/src/data/BOM_climate/monthly_mean_temp", "meanmay.txt"))
mean_temp_jun <- raster(here("project/src/data/BOM_climate/monthly_mean_temp", "meanjun.txt"))
mean_temp_jul <- raster(here("project/src/data/BOM_climate/monthly_mean_temp", "meanjul.txt"))
mean_temp_aug <- raster(here("project/src/data/BOM_climate/monthly_mean_temp", "meanaug.txt"))
mean_temp_sep <- raster(here("project/src/data/BOM_climate/monthly_mean_temp", "meansep.txt"))
mean_temp_oct <- raster(here("project/src/data/BOM_climate/monthly_mean_temp", "meanoct.txt"))
mean_temp_nov <- raster(here("project/src/data/BOM_climate/monthly_mean_temp", "meannov.txt"))
mean_temp_dec <- raster(here("project/src/data/BOM_climate/monthly_mean_temp", "meandec.txt"))

mean_rain_annual <- raster(here("project/src/data/BOM_climate/mean_rainfall", "rnozan.txt"))
mean_rain_dry <- raster(here("project/src/data/BOM_climate/mean_rainfall", "rnozdry.txt"))
mean_rain_wet <- raster(here("project/src/data/BOM_climate/mean_rainfall", "rnozwet.txt"))

mean_rain_jan <- raster(here("project/src/data/BOM_climate/monthly_mean_rainfall", "rainjan.txt"))
mean_rain_feb <- raster(here("project/src/data/BOM_climate/monthly_mean_rainfall", "rainfeb.txt"))
mean_rain_mar <- raster(here("project/src/data/BOM_climate/monthly_mean_rainfall", "rainmar.txt"))
mean_rain_apr <- raster(here("project/src/data/BOM_climate/monthly_mean_rainfall", "rainapr.txt"))
mean_rain_may <- raster(here("project/src/data/BOM_climate/monthly_mean_rainfall", "rainmay.txt"))
mean_rain_jun <- raster(here("project/src/data/BOM_climate/monthly_mean_rainfall", "rainjun.txt"))
mean_rain_jul <- raster(here("project/src/data/BOM_climate/monthly_mean_rainfall", "rainjul.txt"))
mean_rain_aug <- raster(here("project/src/data/BOM_climate/monthly_mean_rainfall", "rainaug.txt"))
mean_rain_sep <- raster(here("project/src/data/BOM_climate/monthly_mean_rainfall", "rainsep.txt"))
mean_rain_oct <- raster(here("project/src/data/BOM_climate/monthly_mean_rainfall", "rainoct.txt"))
mean_rain_nov <- raster(here("project/src/data/BOM_climate/monthly_mean_rainfall", "rainnov.txt"))
mean_rain_dec <- raster(here("project/src/data/BOM_climate/monthly_mean_rainfall", "raindec.txt"))

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
crs(mean_rain_jan) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(mean_rain_feb) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(mean_rain_mar) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(mean_rain_apr) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(mean_rain_may) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(mean_rain_jun) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(mean_rain_jul) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(mean_rain_aug) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(mean_rain_sep) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(mean_rain_oct) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(mean_rain_nov) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(mean_rain_dec) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(mean_temp_jan) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(mean_temp_feb) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(mean_temp_mar) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(mean_temp_apr) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(mean_temp_may) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(mean_temp_jun) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(mean_temp_jul) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(mean_temp_aug) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(mean_temp_sep) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(mean_temp_oct) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(mean_temp_nov) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs(mean_temp_dec) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# load SA2 regions
SA2 <- readOGR(dsn  = here("project/src/data/SA2", "SA2_2016_AUST.shp"), layer = "SA2_2016_AUST")
proj4string(SA2) <- proj4string(mean_temp_annual) 
  
# load agricultural landuses 
landuse <- readOGR(dsn  = here("project/src/data/landuse", "CLUM_Commodities_2018_v2.shp"), layer = "CLUM_Commodities_2018_v2")
proj4string(landuse) <- proj4string(mean_temp_annual)

# load irrigation and variety data - already tidy
variety_irrigation <- read_csv(here::here("project/src/data/variety_irrigation", "variety_irrigation_geo.csv"))

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
lu_mean_temp_jan <- raster::extract(mean_temp_jan, landuse_grapes_centroids)
lu_mean_temp_feb <- raster::extract(mean_temp_feb, landuse_grapes_centroids)
lu_mean_temp_mar <- raster::extract(mean_temp_mar, landuse_grapes_centroids)
lu_mean_temp_apr <- raster::extract(mean_temp_apr, landuse_grapes_centroids)
lu_mean_temp_may <- raster::extract(mean_temp_may, landuse_grapes_centroids)
lu_mean_temp_jun <- raster::extract(mean_temp_jun, landuse_grapes_centroids)
lu_mean_temp_jul <- raster::extract(mean_temp_jul, landuse_grapes_centroids)
lu_mean_temp_aug <- raster::extract(mean_temp_aug, landuse_grapes_centroids)
lu_mean_temp_sep <- raster::extract(mean_temp_sep, landuse_grapes_centroids)
lu_mean_temp_oct <- raster::extract(mean_temp_oct, landuse_grapes_centroids)
lu_mean_temp_nov <- raster::extract(mean_temp_nov, landuse_grapes_centroids)
lu_mean_temp_dec <- raster::extract(mean_temp_dec, landuse_grapes_centroids)
lu_mean_rain_annual <- raster::extract(mean_rain_annual, landuse_grapes_centroids)
lu_mean_rain_dry <- raster::extract(mean_rain_dry, landuse_grapes_centroids)
lu_mean_rain_wet <- raster::extract(mean_rain_wet, landuse_grapes_centroids)
lu_mean_rain_jan <- raster::extract(mean_rain_jan, landuse_grapes_centroids)
lu_mean_rain_feb <- raster::extract(mean_rain_feb, landuse_grapes_centroids)
lu_mean_rain_mar <- raster::extract(mean_rain_mar, landuse_grapes_centroids)
lu_mean_rain_apr <- raster::extract(mean_rain_apr, landuse_grapes_centroids)
lu_mean_rain_may <- raster::extract(mean_rain_may, landuse_grapes_centroids)
lu_mean_rain_jun <- raster::extract(mean_rain_jun, landuse_grapes_centroids)
lu_mean_rain_jul <- raster::extract(mean_rain_jul, landuse_grapes_centroids)
lu_mean_rain_aug <- raster::extract(mean_rain_aug, landuse_grapes_centroids)
lu_mean_rain_sep <- raster::extract(mean_rain_sep, landuse_grapes_centroids)
lu_mean_rain_oct <- raster::extract(mean_rain_oct, landuse_grapes_centroids)
lu_mean_rain_nov <- raster::extract(mean_rain_nov, landuse_grapes_centroids)
lu_mean_rain_dec <- raster::extract(mean_rain_dec, landuse_grapes_centroids)
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

# geocode variety and irrigation GI regions from name
# register_google(key = "")
# geocodes <- geocode(variety_irrigation$geo_desc)
# write.table(geocodes, file = here("project/src/output/geocodes.csv"), sep = ",", col.names = TRUE, row.names = FALSE, append = F, quote = FALSE)

geocodes <- read_csv(here::here("project/src/output", "geocodes.csv"))
variety_irrigation_points <- SpatialPointsDataFrame(coords = geocodes, data = variety_irrigation, proj4string = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))


# merge climate, landuses and yields 
landuse_climate <- data.frame(landuse_grapes_centroids@data, lu_mean_temp_annual, lu_mean_temp_winter, lu_mean_temp_summer, lu_max_temp_annual, lu_max_temp_winter, lu_max_temp_summer, lu_min_temp_annual, lu_min_temp_winter, lu_min_temp_summer, lu_mean_temp_jan, lu_mean_temp_feb, lu_mean_temp_mar, lu_mean_temp_apr, lu_mean_temp_may, lu_mean_temp_jun, lu_mean_temp_jul, lu_mean_temp_aug, lu_mean_temp_sep, lu_mean_temp_oct, lu_mean_temp_nov, lu_mean_temp_dec,
                              lu_mean_rain_annual, lu_mean_rain_dry, lu_mean_rain_wet, lu_mean_solar_annual, lu_mean_solar_dry, lu_mean_solar_wet, lu_bulk_density, lu_carbon, lu_clay, lu_nitrogen, lu_ph, lu_phosphorus, lu_sand, lu_silt, lu_water_capacity, lu_mean_rain_jan,
                              lu_mean_rain_feb, lu_mean_rain_mar, lu_mean_rain_apr, lu_mean_rain_may, lu_mean_rain_jun, lu_mean_rain_jul, lu_mean_rain_aug, lu_mean_rain_sep, lu_mean_rain_oct, lu_mean_rain_nov, lu_mean_rain_dec)

landuse_climate_yield <- left_join(landuse_climate, region_yield_area_SA2)

# tidy table
yield_by_landuse <- landuse_climate_yield %>% 
  dplyr::select(-Broad_type, -Source_yr, -Lucodev8n, -Shape_Leng, -Shape_Area, -optional, -region.x) %>% 
  filter(!is.na(yield))

# summarise for results by region
yield_by_region <- yield_by_landuse %>% 
  group_by(code) %>% 
  summarise(region = first(region), state = first(State),lon = mean(lon), lat = mean(lat), grape_area_region = mean(area), grape_area_landuse = sum(Area_ha), lu_mean_temp_annual = mean(lu_mean_temp_annual), lu_mean_temp_winter = mean(lu_mean_temp_winter), lu_mean_temp_summer = mean(lu_mean_temp_summer), lu_max_temp_annual = mean(lu_max_temp_annual), lu_max_temp_winter = mean(lu_max_temp_winter), lu_max_temp_summer = mean(lu_max_temp_summer), lu_min_temp_annual = mean(lu_min_temp_annual), lu_min_temp_winter = mean(lu_min_temp_winter), lu_min_temp_summer = mean(lu_min_temp_summer), lu_mean_temp_jan = mean(lu_mean_temp_jan), lu_mean_temp_feb = mean(lu_mean_temp_feb), lu_mean_temp_mar = mean(lu_mean_temp_mar), lu_mean_temp_apr = mean(lu_mean_temp_apr), lu_mean_temp_may = mean(lu_mean_temp_may), lu_mean_temp_jun = mean(lu_mean_temp_jun), lu_mean_temp_jul = mean(lu_mean_temp_jul), lu_mean_temp_aug = mean(lu_mean_temp_aug), lu_mean_temp_sep = mean(lu_mean_temp_sep), lu_mean_temp_oct = mean(lu_mean_temp_oct), lu_mean_temp_nov = mean(lu_mean_temp_nov), lu_mean_temp_dec = mean(lu_mean_temp_dec), lu_mean_rain_annual = mean(lu_mean_rain_annual), lu_mean_rain_dry = mean(lu_mean_rain_dry), lu_mean_rain_wet = mean(lu_mean_rain_wet), lu_mean_rain_jan = mean(lu_mean_rain_jan), lu_mean_rain_feb = mean(lu_mean_rain_feb), lu_mean_rain_mar = mean(lu_mean_rain_mar), lu_mean_rain_apr = mean(lu_mean_rain_apr), lu_mean_rain_may = mean(lu_mean_rain_may), lu_mean_rain_jun = mean(lu_mean_rain_jun), lu_mean_rain_jul = mean(lu_mean_rain_jul), lu_mean_rain_aug = mean(lu_mean_rain_aug), lu_mean_rain_sep = mean(lu_mean_rain_sep), lu_mean_rain_oct = mean(lu_mean_rain_oct), lu_mean_rain_nov = mean(lu_mean_rain_nov), lu_mean_rain_dec = mean(lu_mean_rain_dec), lu_mean_solar_annual = mean(lu_mean_solar_annual), lu_mean_solar_dry = mean(lu_mean_solar_dry), lu_mean_solar_wet = mean(lu_mean_solar_wet), lu_bulk_density = mean(lu_bulk_density), lu_carbon = mean(lu_carbon), lu_clay = mean(lu_clay), lu_nitrogen = mean(lu_nitrogen), lu_ph = mean(lu_ph), lu_phosphorus = mean(lu_phosphorus), lu_sand = mean(lu_sand), lu_silt = mean(lu_silt), lu_water_capacity = mean(lu_water_capacity), yield = mean(yield)) 

yield_by_region_coords <- data.frame(yield_by_region$lon, yield_by_region$lat) 
yield_by_region_points <- SpatialPointsDataFrame(coords = yield_by_region_coords, data = yield_by_region, proj4string = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# join irrigation and variety by nearest points
nn_output <- get.knnx(coordinates(variety_irrigation_points), coordinates(yield_by_region_coords), 1)
yield_by_region_1 <- data.frame(yield_by_region, index = nn_output$nn.index)
variety_irrigation_1 <- mutate(variety_irrigation, index = as.numeric(rownames(variety_irrigation)))

yield_by_region_1 <- left_join(yield_by_region_1, variety_irrigation_1, by = "index")

# output tables  
write.table(yield_by_landuse, file = here("project/src/output/yield_by_landuse.csv"), sep = ",", col.names = TRUE, row.names = FALSE, append = F, quote = FALSE)    
saveRDS(yield_by_landuse,file = here("project/src/output/yield_by_landuse.rds"))
write.table(yield_by_region_1, file = here("project/src/output/yield_by_region.csv"), sep = ",", col.names = TRUE, row.names = FALSE, append = F, quote = FALSE)
saveRDS(yield_by_region_1,file = here("project/src/output/yield_by_region.rds"))

ggplot(yield_by_region, aes(x = lu_mean_temp_dec, y = yield)) +
  geom_point()



# Modeling 

yield_by_region_1 <- yield_by_region_1 %>% 
                     filter(state == 'NSW' | state == 'WA' | state == 'Tas' | state =='Vic' | state =='SA') 

# set train and test ----

train_size <- floor(0.7 * nrow(yield_by_region_1))

set.seed(10) 
train_n <- sample(seq_len(nrow(yield_by_region_1)), size = train_size)

train <- yield_by_region_1[train_n, ]
test <- yield_by_region_1[-train_n, ]

# check sizes

nrow(yield_by_region_1) == nrow(train) + nrow(test)


# Diagnostics a bit more on EDA 

summary(yield_by_region_1)                       # stats   

skewness(yield_by_region_1$yield)                # skewness function [- left ; + right]

grubbs.test(yield_by_region_1$yield, type = 10, opposite = FALSE, two.sided = FALSE)

boxB(yield_by_region_1$yield, k=1.5, method='resistant', weights=NULL, id=NULL,
     exclude=NA, logt=FALSE)

# Character, identifies the method to be used: method="resistant" provides the
# ‘standard’ boxplot fences; method="asymmetric" is a modification of standard
# method to deal with (moderately) skewed data; method="adjbox" uses Hubert
# and Vandervieren (2008) adjusted boxplot for skewed distributions.


x <- model.matrix(~., train[,!(colnames(train) == "yield")]) # Lasso Regularization Algorithm 
y <- train$yield
cv.out <- cv.glmnet(x, y, alpha=1, type.measure = "mse" )
lambda_min <- cv.out$lambda.min
lambda_1se <- cv.out$lambda.1se
coef <- as.matrix(coef(cv.out,s=lambda_1se))
coef2 <- as.data.frame(as.table(coef))
coef2 <- coef2 %>% 
         select(-Var2) %>%
         filter(Freq != 0) %>%
         rename(Variable = Var1, Coeficients = Freq)

coef2

# The output shows that only those variables that had been determined to be significant 
# on the basis of p-values have  non-zero coefficients. The coefficients of all other 
# variables have been set to zero by the algorithm.



# Method 1

# Select and Fit for Variables

train1 <- train %>% 
  dplyr::select(lon:total_water_used)

test1 <- test

fit_select <- glm(yield ~ 
                 
                 # lon +
                 lat +
                 lu_mean_temp_jan +
                 lu_mean_temp_feb +
                 # lu_mean_temp_mar +
                 # lu_mean_temp_apr +
                 # lu_mean_temp_may +
                 # lu_mean_temp_jun +
                 # lu_mean_temp_jul +
                 # lu_mean_temp_aug +
                 lu_mean_temp_sep +
                 lu_mean_temp_oct +
                 lu_mean_temp_nov +
                 lu_mean_temp_dec +
                 
                 lu_mean_rain_jan +
                 lu_mean_rain_feb +
                 # lu_mean_rain_mar +
                 # lu_mean_rain_apr +
                 # lu_mean_rain_may +
                 # lu_mean_rain_jun +
                 # lu_mean_rain_jul +
                 # lu_mean_rain_aug +
                 lu_mean_rain_sep +
                 lu_mean_rain_oct +
                 lu_mean_rain_nov +
                 lu_mean_rain_dec +
                 
                 # lu_mean_solar_annual +
                 # lu_mean_solar_dry +
                 # lu_mean_solar_wet +
                 lu_bulk_density +
                 # lu_carbon +
                 lu_clay +
                 lu_nitrogen +
                 lu_ph +
                 lu_phosphorus +
                 # lu_sand +
                 lu_silt +
                 lu_water_capacity +
                 total_water_used,
               
               
               # family = inverse.gaussian(link = "log"),
               # family = inverse.gaussian(link = "1/mu^2"),
               # family = Gamma(link = "inverse"),
               family = gaussian(link = "identity"),
               
               data = train1)

summary(fit_select) # Stats

vif(fit_select) # Retain model if vif (Variable Inflation Factors) of each feature is below 5. 

resid_panel(fit_select, plots = "all") # Diagnostic Plots

train1$residuals <- residuals(fit_select) # Calculate residuals values of model for training data 

train1$predicted <- predict(fit_select, train1, type="response") # Calculate predicted values of model for training data 

rmse_train1 <- sqrt(mean(train1$residuals ** 2)) # Root Mean Squared Error for training data 

test1$predicted <- predict(fit_select, test1) # Calculate residuals values of model for training data 

test1$residuals <- test1$yield - test1$predicted #  Calculate residuals values of model for test data 

rmse_test1 <- sqrt(mean(test1$residuals ** 2)) # Root Mean Squared Error for test data 

rmse_train1/rmse_test1 # Check RMSEs shouldnt exceed 15% of each other


# Method 2

train_feat <- train %>% 
  dplyr::select(lon:total_water_used)


# setup trainControl object
control <- trainControl(  method = "cv",              # cross-validation
                          number = 5,                 # 5 folds 
                          savePredictions = "final",
                          allowParallel = TRUE
)

# setup modelling parameters and run ----
fit <- train(   x = train_feat,
                y = train$yield, 
                method = "lm", 
                metric = "RMSE",
                trControl = trainControl(method = "cv",number=5), 
                preProc = c("center","scale")
)

print(fit)
summary(fit)

# plot variable importance
ggplot(varImp(fit))

# predict on our test set ----
test_feat <- test %>% 
  dplyr::select(lu_mean_temp_annual:lu_water_capacity)

pred <- predict(fit, test_feat)

# calculate rmse and scatter index
rmse <- rmse(pred, test$yield)
SI <- rmse/mean(yield_by_region_1$yield)





