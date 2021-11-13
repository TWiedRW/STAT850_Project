library(sf)
library(ggplot2)
library(htmltools)
library(leaflet)
library(leaflet.providers)

firezones <- st_read("Fire_planning_zones.kml")
stations <- st_read("Fire_stations.kml")
firezone_info <- read.csv('Firezone_Info.csv')
engine_times <- st_read("Fire_station_travel_times.kml")
station_info <- read.csv('Station_Data.csv')

#firezone_engine_df <- tibble(
#  engine <- purrr::map_chr(purrr::map(kml, Document), Folderpurrr::map(Folder), purrr::map(Placemark[5]), purrr::map(ExtendedData), purrr::map(SchemaData), purrr::map(SimpleData[2]), "name")

station_info$label <- with(station_info, paste(
  "<p> <b>", station_info$Station_Num, "</b> </br>",
  station_info$Station_Loc,
  "</p>"))

firezone_info$col=sample(c('cyan','yellow','green','blue','grey','orange'),nrow(firezone_info),1)

  m <- leaflet()
  m <- addTiles(m)
  m <- addMarkers(m, data=stations, popup=(station_info$label))
  m <- addPolygons(m, data= firezones, stroke=TRUE, color='black', weight=2, opacity= 0.8, fill=TRUE, fillColor = firezone_info$col, fillOpacity = 0.3, label= (firezone_info$Engine))
  m
  

  times <- leaflet()
  times <- addTiles(times)
  times <- addPolygons(times, data=engine_times, stroke=TRUE, color='black', weight=2, fill=TRUE)
  times
  