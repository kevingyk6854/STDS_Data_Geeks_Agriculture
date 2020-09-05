### load and filter large shapefiles ###

# whole of Australia files are beyond the Github 50 MB limit so filtering only WA and SA
# can potentially work with larger files in the future using
# https://git-lfs.github.com/ and https://cran.r-project.org/web/packages/piggyback/vignettes/intro.html

library(here)
library(rgdal)

### region data ###
# unzipped file from https://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_sa2_2016_aust_shape.zip&1270.0.55.001&Data%20Cubes&A09309ACB3FA50B8CA257FED0013D420&0&July%202016&12.07.2016&Latest
# in data folder

# load shapefile and filter
SA2 <- readOGR(dsn  = here("project/src/data/1270055001_sa2_2016_aust_shape", "SA2_2016_AUST.shp"), layer = "SA2_2016_AUST")
SA2_SA_WA <- subset(SA2, STE_CODE16 == 4 | STE_CODE16 == 5)

### landuse data ###
# unzipped file from https://www.agriculture.gov.au/sites/default/files/documents/clum_commodities_2018_v2.zip in data folder
# load shapefile and filter 
landuse <- readOGR(dsn  = here("project/src/data/clum_commodities_2018_v2/Spatial_data_zip", "CLUM_Commodities_2018_v2.shp"), layer = "CLUM_Commodities_2018_v2")
landuse_SA_WA <- subset(landuse, State == "SA" | State == "WA")

### save filtered datasets ###
writeOGR(SA2_SA_WA, dsn = here("project/src/data", "SA2_2016_SA_WA.shp"), layer = "SA2_2016_AUST",  driver="ESRI Shapefile")
writeOGR(landuse_SA_WA, dsn = here("project/src/data", "landuse_SA_WA.shp"), layer = "landuse",  driver="ESRI Shapefile")
