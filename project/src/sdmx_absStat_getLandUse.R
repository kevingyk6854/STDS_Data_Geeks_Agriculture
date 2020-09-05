#
# Retrieve data from ABS.Stat using SDMX
# on Land Use
#
library(magrittr)
library(dplyr)

# get complete URL from http://stat.data.abs.gov.au/index.aspx#
# select from left panel: INDUSTRY -> Agriculture -> Land Use -> 
# Choose sdmx as Export format
abs_stat_url <- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/ABS_LAND_USE/T+1+2+3+4_6+51+52+53+55_56+5R+7_8+9+101+102+103+104.210+200+205+1+2+3+4_6+51+52+53+55_56+5R+7_8+9+101+102+103+104.1+2.0+4+401+402+403+404+405+406+407+408/all?startTime=2011&endTime=2011"


abs.land_use <- readSDMX(abs_stat_url)
as.data.frame(abs.land_use)

str(abs.land_use)

# get DSD
# decompose the above URL into parameters in calling readSDMX
abs.land_use_dsd <- readSDMX(#we are going to use the same parameters as we filtered on to get the direct url - make sure you can see how it all came out
  providerId ="ABS", 
  resource ="data", 
  flowRef ="ABS_LAND_USE",
  key ="/T+1+2+3+4_6+51+52+53+55_56+5R+7_8+9+101+102+103+104.210+200+205+1+2+3+4_6+51+52+53+55_56+5R+7_8+9+101+102+103+104.1+2.0+4+401+402+403+404+405+406+407+408/all", 
  key.mode ="SDMX",
  dsd =TRUE) #this is where we say we want the data definition

# transform into data frame
land_use <- as.data.frame(abs.land_use_dsd)

glimpse(land_use)

# EDA
# Get only "Agriculture Cropping"
land_use %<>% dplyr::filter(LU_11 == "51")

unique(land_use$obsTime)
unique(land_use$TIME_FORMAT)
land_use %<>% dplyr::select(-obsTime, -LU_11, -TIME_FORMAT)

# Get only "Agriculture Cropping" in LU_11NC as well
land_use %>% filter(LU_11NC == "51")

colnames(land_use)
# rename columns
names(land_use) <- c("Land Use Net Change", "Measure", "Region", "Value", "Flags")
