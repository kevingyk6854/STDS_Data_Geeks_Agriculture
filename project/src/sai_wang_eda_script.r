library(here)
library(tidyverse) 
library(stringr)
library(ggplot2)
library(rgdal)
library(raster)
library(rsdmx)
library(sp)
library(slga) # retrieve data from the Soil and Landscape Grid of Australia
library(readr)
library(corrplot)
library(GGally)
library(lattice)
library(geosphere)
library(mapview)

# read data from csv file
yield_by_region <- read_csv(here::here("project/src/output/", "yield_by_region.csv"))

# Hayden plot test
ggplot(yield_by_region, aes(x = lu_water_capacity, y = yield)) +
  geom_point()

# extract only numeric columns from dataset
yield_by_region_num <- yield_by_region %>%
  dplyr::select(4:30)
# view(yield_by_region_num)

summary(yield_by_region_num)

# http://www.cookbook-r.com/Graphs/Plotting_distributions_(ggplot2)/

myPlotFunc <- function(df, folder){
  col = colnames(df)
  for (i in 1:length(col)) {
    pdf(paste(folder, "histo_", col[i], ".pdf", sep=""))
    h <- ggplot(data = df, 
                aes(df[,i])) + 
      geom_histogram(aes(y=..density..), 
                     bins=30, color="black", fill="white") +
      geom_density(alpha=.2, fill="#FF6666") + labs(x=col[i])
    print(h)
    dev.off()
    
    pdf(paste(folder, "boxplot_", col[i], ".pdf", sep=""))
    b <- ggplot(data = df,
                aes(y=df[,i])) +
      geom_boxplot(outlier.colour="red", outlier.shape=8,
                   outlier.size=4, width=0.4) +
      xlim(-0.5, 0.5) + labs(x=col[i], y = "")
    print(b)
    dev.off()
  }
  
  
  pdf(paste(folder, "correlation.pdf", sep=""))
  cor_plot <- corrplot(cor(df, use="pairwise.complete.obs"), method = "number")
  print(cor_plot)
  dev.off()
  
  return(0)
}

myPlotFunc(yield_by_region_num, "project/src/plot/grape/")

df = yield_by_region_num[,
     c('lu_mean_temp_annual','lu_mean_rain_annual', 'lu_mean_solar_annual', 
       "lu_bulk_density", "lu_carbon", "lu_clay", "lu_nitrogen", "lu_ph", 
       "lu_phosphorus", "lu_sand", "lu_silt", "lu_water_capacity",
       "yield")]
folder = "project/src/plot/grape/"
col = colnames(df)
for (i in 1:length(col)) {
  pdf(paste(folder, "histo_", col[i], ".pdf", sep=""))
  h <- ggplot(data = df, 
              aes(unlist(df[,i]))) + 
    geom_histogram(aes(y=..density..), 
                   bins=30, color="black", fill="white") +
    geom_density(alpha=.2, fill="#FF6666") + labs(x=col[i])
  print(h)
  dev.off()
  
  pdf(paste(folder, "boxplot_", col[i], ".pdf", sep=""))
  b <- ggplot(data = df,
              aes(y=unlist(df[,i]))) +
    geom_boxplot(outlier.colour="red", outlier.shape=8,
                 outlier.size=4, width=0.4) +
    xlim(-0.5, 0.5) + labs(x=col[i], y = "")
  print(b)
  dev.off()
}

class(col)
for (i in 1:length(col)) {
  print(i)
  col[i] <- str_replace(col[i], "lu_mean_", "")
  col[i] <- str_replace(col[i], "lu_", "")
}

colnames(df) = col


pdf(paste(folder, "correlation.pdf", sep=""))
cor_plot <- corrplot(cor(df, use="pairwise.complete.obs"), method = "number", number.cex=0.75)
print(cor_plot)
dev.off()




pdf(paste(folder, "climate_pair.pdf", sep=""))
lowerFn <- function(data, mapping, method = "lm", ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(colour = "blue") +
    geom_smooth(method = method, color = "red", ...)
  p
}
#https://stackoverflow.com/questions/30858337/how-to-customize-lines-in-ggpairs-ggally
ggpairs(
  df, columns=c(1,2,3,13), lower = list(continuous = wrap(lowerFn, method = "lm")),
  diag = list(continuous = wrap("barDiag", colour = "blue")),
  upper = list(continuous = wrap("cor", size = 6))
)
dev.off()

pdf(paste(folder, "Soil_pair.pdf", sep=""))
lowerFn <- function(data, mapping, method = "lm", ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(colour = "blue") +
    geom_smooth(method = method, color = "red", ...)
  p
}

ggpairs(
  df, columns=c(4:13), lower = list(continuous = wrap(lowerFn, method = "lm")),
  diag = list(continuous = wrap("barDiag", colour = "blue")),
  upper = list(continuous = wrap("cor", size = 2))
)
dev.off()
