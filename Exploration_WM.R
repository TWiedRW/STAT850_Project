library(sf)
library(ggplot2)
library(htmltools)
library(leaflet)
library(leaflet.providers)
library(RColorBrewer)
library(lubridate)

firezones <- st_read("Fire_planning_zones.kml")
stations <- st_read("Fire_stations.kml")
firezone_info <- read.csv('Firezone_Info.csv')
engine_times <- st_read("Fire_station_travel_times.kml")
station_info <- read.csv('Station_Data.csv')

#firezone_engine_df <- tibble(
#  engine <- purrr::map_chr(purrr::map(kml, Document), Folderpurrr::map(Folder), purrr::map(Placemark[5]), purrr::map(ExtendedData), purrr::map(SchemaData), purrr::map(SimpleData[2]), "name")


#Trying to find new ways to color the original leaflet.
firezone_info$col=sample(c('cyan','yellow','green','blue','grey','orange'),nrow(firezone_info),1)


#Create a new column that just has turnout time as a single numeral
apparatus2$turn_out_seconds <- period_to_seconds(hms(apparatus2$turn_out_time)) 


#This code is used to find total number of dispatches for each station, as well as average turnout time for each station
df <- data.frame(apparatus2$station, times(apparatus2$turn_out_seconds))
turnout_average <- aggregate(apparatus2$turn_out_seconds ~ apparatus2$station, data=df, mean)
dispatch_count <- aggregate(.~apparatus2$station, data=df, FUN=length)


#Combining Stations 0 and 1 into a single station
dispatch_count[2, 2] = 8490

#Creating a label for the markers in the map
station_info$label <- with(station_info, paste(
  "<p> <b>", station_info$Station_Num, "</b> </br>",
  station_info$Station_Loc,
  "</p>"))

#Creating a label for the polygons for the dispatch map
firezone_info$label <- with(firezone_info, paste(
  "<p> <b>","Dispatches =", dispatch_count$apparatus2.station,
  "</p>"))



#This is the base map that shows each station with corresponding firezones and their engines
base_map <- leaflet()
base_map <- addTiles(base_map)
base_map <- addMarkers(base_map, data=stations, popup=(station_info$label))
base_map <- addPolygons(base_map, data= firezones, stroke=TRUE, color='black', weight=2, opacity= 0.8, fill=TRUE, 
                            fillColor = 'red', fillOpacity = 0.3, label= (firezone_info$Engine))
base_map


#Creating a palette function to create gradient for dispatches and turnout time
pal_disp <- colorNumeric(
  palette = "Blues",
  domain = dispatch_count$apparatus2.station)

pal_turnout <- colorNumeric(
  palette = "PuOr",
  domain = (0:120))

#This map is for the count of dispatches from the stations, markers are currently commented out
dispatch_map <- leaflet()
dispatch_map <- addTiles(dispatch_map)
#dispatch_map <- addMarkers(dispatch_map, data=stations, popup=(station_info$label))
dispatch_map <- addPolygons(dispatch_map, data= firezones, stroke=TRUE, color='black', weight=2, opacity= 0.8, fill=TRUE, 
                   fillColor = ~pal_disp(dispatch_count$apparatus2.station), fillOpacity = 1, label= (firezone_info$Engine))
dispatch_map <-  addLegend(dispatch_map, position="bottomright", pal= pal_disp, values= (0:9000), title= 'Total Dispatches for 2019', opacity=1)
dispatch_map

#This map is for the average time for turnout from each station, markers currently commented out
turnout_map <- leaflet()
turnout_map <- addTiles(turnout_map)
#turnout_map <- addMarkers(turnout_map, data=stations, popup=(station_info$label))
turnout_map <- addPolygons(turnout_map, data= firezones, stroke=TRUE, color='black', weight=2, opacity= 0.8, fill=TRUE, 
                            fillColor = ~pal_turnout(turnout_average$`apparatus2$turn_out_seconds`), fillOpacity = 1, label= (firezone_info$Engine))
turnout_map <-  addLegend(turnout_map, position="bottomright", pal= pal_turnout, values= (0:120), title= 'Avg Turnout Time for 2019', opacity=1)
turnout_map
    
    

  
# First preliminary attempt at making a map for each station with its range in certain times from the station. Will likely need to integrate Shiny for this.
  times <- leaflet()
  times <- addTiles(times)
  times <- addPolygons(times, data=engine_times, stroke=TRUE, color='black', weight=2, fill=TRUE)
  times
  