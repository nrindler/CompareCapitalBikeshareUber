# MAPPING

library(leaflet)
library(rgdal)
library(sp)
library(jsonlite)
library(raster)
library(rgeos)
library(htmlwidgets)

# Load geo data
load("Data/census_tracts_sp.Rdata")
load("Data/census_tracts_spdf.Rdata")
load("Data/cabi_stations_geo.Rdata")
#load("Data/census_tract_centroids.rData")

# Load Cabi/Uber simulation results
load("Data/simulation_results.rData")


# List of DC area census tracts (from Uber)
census = fromJSON("Raw_Data/washington_DC_censustracts.json")
tract_id = census$features$properties$MOVEMENT_ID
polys = census$features$geometry$coordinates



# MAP - ENTIRE UBER / CABI COVERAGE AREA

labels <- sprintf(
  "<strong>Start Tract </strong>%s",
  tract_id
) %>% lapply(htmltools::HTML)

m = leaflet() %>% 
  setView(lng=,-77.0454855, lat=38.9143523, zoom=11) %>% 
  addProviderTiles(providers$OpenStreetMap) %>% 
  addPolygons(data=tracts, weight=2,
              label=labels,
              fillColor = "blue",
              color="black",
              fillOpacity = 0.1,
              highlight = highlightOptions(
                weight = 5,
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = F),
              labelOptions = labelOptions(#noHide=T, textOnly=T, 
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "12px",
                direction = "auto")) %>% 
  addCircleMarkers(lng=cabi_tract$lon, lat=cabi_tract$lat, 
                   radius=7, color="red", fillOpacity=1,
                   label=cabi_tract$station,
             clusterOptions = markerClusterOptions())
m

saveWidget(m, file="overview_map.html")




# MAP travel time for top cabi start tract is 203 (Farragut Square)

sim203 = simulation_results[simulation_results$start_tract==203,]
sim203$cabi_win_rate = 1 - sim203$uber_win_rate


tracts_df@data$id = as.numeric(as.character(tracts_df@data$id))
tracts_df@data = tracts_df@data %>% 
  left_join(sim203[sim203$time=="3PM",], by=c("id"="end_tract"))

bins <- seq(0,1.05,0.15)
bins[8] = 1.00
pal <- colorBin("YlOrRd", domain = sim203$cabi_win_rate, bins = bins)
#pal = colorNumeric("YlOrRd", domain = tracts_df@data$cabi_win_rate)
tracts_df@data$pal = pal(tracts_df@data$cabi_win_rate)

# MAKE STARTING CENSUS TRACT (203) BRIGHT RED
tracts_df@data[tracts_df@data$id==203,"pal"]= "#0033ff"

#bins = round(bins*100)
#generate color buckets
leg.txt <- c(paste("<",bins[2],sep=""), 
             paste(bins[2],bins[3],sep="-"),
             paste(bins[3],bins[4],sep="-"),
             paste(bins[4],bins[5],sep="-"),
             paste(bins[6],bins[7],sep="-"),
             paste(bins[5],bins[6],sep="-"),
             paste(">",bins[7],sep=""))

cabi_sim_tracts = unique(c(unique(simulation_results$start_tract),
                           unique(simulation_results$end_tract)))

# Subset map to only census tracts within CaBi service area
tracts_sub = tracts_df[which(tracts_df@data$id %in% cabi_sim_tracts),]

labels <- sprintf(
  "<strong>Start Tract </strong>203 (Farragut Square)</br>
   <strong>End Tract </strong> %d</br>
   <strong>CaBi Win Rate </strong> %.2f pct</br>
   <strong>Uber Median (Mins)</strong> %.2f</br>
   <strong>CaBi Median </strong> %.2f</br>
   <strong>Uber Avg Winning Margin </strong> %.2f</br>
   <strong>CaBi Avg Winning Margin </strong> %.2f</br>
   <strong># CaBi Rides Used </strong> %d",
  tracts_sub@data$id, 
  tracts_sub@data$cabi_win_rate*100,
  tracts_sub@data$uber_median/60,
  tracts_sub@data$cabi_median/60,
  tracts_sub@data$uber_avg_winning_margin/60,
  tracts_sub@data$cabi_avg_winning_margin/60,
  tracts_sub@data$nobs
) %>% lapply(htmltools::HTML)

map = leaflet(tracts_sub) %>% 
  setView(lng=,-77.0454855, lat=38.9143523, zoom=12) %>% 
  addProviderTiles(providers$OpenStreetMap) %>% 
  addPolygons(fillColor = tracts_sub@data$pal,
              weight = 2,
              opacity = 1,
              label=labels,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,              
              highlight = highlightOptions(
                weight = 5,
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = F),
              labelOptions = labelOptions(#noHide=T, textOnly=T, 
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "12px",
                direction = "auto")) %>% 
  addCircleMarkers(lng=cabi_tract$lon, lat=cabi_tract$lat, 
                   radius=7, color="black", fillOpacity=1,
                   label=cabi_tract$station,
                   clusterOptions = markerClusterOptions(),
                   group="CaBi Stations") %>% 
  addLegend("bottomright", pal = pal, values=leg.txt,
            title = "Cabi Win Rate",
            labFormat = labelFormat(suffix = "%",
                                    transform = function(x) 100 * x),
            opacity = 1) %>% 
  # Layers control
  addLayersControl(
    #baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
    overlayGroups = "CaBi Stations", #c("Quakes", "Outline"),
    options = layersControlOptions(collapsed = FALSE))
map

saveWidget(map, file="map.html")





