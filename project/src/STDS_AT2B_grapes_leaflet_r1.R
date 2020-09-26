library(leaflet)

# create colour palettes for both objects
rain_pal <- colorNumeric("RdYlBu", values(ph))

# create leaflet with Openstreetmap background, vector and raster data overlayed
leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron, options = tileOptions(minZoom = 8, maxZoom = 12)) %>% 
  addPolygons(data = landuse_grapes, weight = 2,  smoothFactor = 0.8, opacity = 0.8, fill = FALSE, color = "green", group = 'land') %>%
  addPolygons(data = SA2, weight = 2,  smoothFactor = 0.8, opacity = 0.8, fill = FALSE, color = "purple", group = 'sa2') %>% 
  addRasterImage(ph, colors = "Spectral", opacity = 0.5, group = 'rain') %>% 
  addLayersControl(
    overlayGroups = c("land", "sa2", "rain"),
    options = layersControlOptions(collapsed = FALSE)
  )