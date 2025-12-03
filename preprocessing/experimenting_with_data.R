library(openair)
library(tidyverse)
library(leaflet)


sample_data <- importAURN(site = "my1", year = 2015:2025, pollutant = c("nox", "no2", "o3", "pm2.5", "pm10"))


aurn_locations <- importMeta(source = "aurn")


leaflet(aurn_locations) |> 
  addTiles() |> 
  addMarkers(lat = aurn_locations$latitude, lng = aurn_locations$longitude, clusterOptions = markerClusterOptions())


# Lets look at the kind of plots that could be made for the time series data 


sample_data |> 
  #filter(lubridate::year(date) == 2025) |> 
  select(date, no2, pm2.5, o3, air_temp) |> 
  dygraph() |> 
  dyRangeSelector() |> 
  dyOptions(stackedGraph = F) |> 
  dyAxis("x", drawGrid = F) |> 
  dyRoller(showRoller = T, rollPeriod = "days")
