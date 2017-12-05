#-----------------------------
#
#  Capital Bikeshare (CaBi) Station Data
#
#  Purpose: Get geo-coordinates (lat/long) for CaBi stations using real time data (no historical data)
#
#  Created by: Nadav Rindler 
#
#  Created on: 2017-10-17
#
#-----------------------------

#setwd("C:\\Users\\nadav.rindler\\OneDrive - American Red Cross\\Training\\UW Data Science Certificate\\DS450\\Capstone_Project")

detach(package:raster, unload = TRUE)
library(jsonlite)
library(dplyr)
library(lubridate)
library(data.table)


# # Download CaBi Region codes from API
# url = "https://gbfs.capitalbikeshare.com/gbfs/en/system_regions.json"
# file.path = "C:\\Users\\nadav.rindler\\OneDrive - American Red Cross\\Training\\UW Data Science Certificate\\DS450\\Capstone_Project\\Raw_Data\\cabi_regions.json"
# print(paste('Getting link at',Sys.Date(),":",url,sep=" "))
# download.file(url, file.path)
# 
# # Import JSON file to R
# regions = fromJSON(file.path)


# Download CaBi station data from API
# url = "https://gbfs.capitalbikeshare.com/gbfs/en/station_information.json"
file.path = "C:\\Users\\nadav.rindler\\OneDrive - American Red Cross\\Training\\UW Data Science Certificate\\DS450\\Capstone_Project\\Raw_Data\\cabi_station_info.json"
# print(paste('Getting link at',Sys.Date(),":",url,sep=" "))
# download.file(url, file.path)

# Import JSON file to R
st = fromJSON(file.path)
current_stations = st$data$stations

current_stations$name = trimws(tolower(current_stations$name), which = "both")

# Load CaBi full data set
load("Data/cabi.Rdata")

# How many stations are there?
all_stations = data.frame(station=levels(cabi_frame$start_st))
all_stations$station = trimws(tolower(as.character(all_stations$station)))
all_stations = all_stations %>% group_by(station) %>% summarize(st = unique(station)) %>% select(-station) %>% rename(station=st)

all_stations$orig_station = all_stations$station

# Drop unknown stations
drops = c(unlist(all_stations[grep("alta",all_stations$station),"station"]),
          "birthday station", "central library")

all_stations = all_stations[-which(all_stations$station %in% drops),]

# Manual data entries to fix inconsistent station names
    # Combine nearby stations when station has shifted by a block
all_stations[all_stations$station=="12th & hayes st /  pentagon city metro","station"] = "pentagon city metro / 12th & s hayes st"
all_stations[all_stations$station=="12th & hayes st","station"] = "pentagon city metro / 12th & s hayes st"
all_stations[all_stations$station=="pentagon city metro / 12th & hayes st","station"] = "pentagon city metro / 12th & s hayes st"
all_stations[all_stations$station=="17th & k st nw [formerly 17th & l st nw]","station"] = "17th & k st nw"
all_stations[all_stations$station=="18th & wyoming ave nw","station"] = "18th st & wyoming ave nw"
all_stations[all_stations$station=="19th & new hampshire ave nw (dupont circle south)","station"] = "20th & o st nw / dupont south"
all_stations[all_stations$station=="23rd & eads","station"] = "23rd & eads st"
all_stations[all_stations$station=="26th & crystal dr","station"] = "27th & crystal dr"
all_stations[all_stations$station=="33rd & water st nw","station"] = "34th & water st nw"
all_stations[all_stations$station=="5th & kennedy st nw","station"] = "4th & kennedy st nw"
all_stations[all_stations$station=="23rd & eads st","station"] = "22nd & eads st"
all_stations[all_stations$station=="17th & rhode island ave nw","station"] = "rhode island & connecticut ave nw"

all_stations[all_stations$station=="18th & bell st","station"] = "crystal city metro / 18th & bell st"
all_stations[all_stations$station=="20th & bell st","station"] = "crystal city metro / 18th & bell st"
all_stations[all_stations$station=="4th & kennedy st nw","station"] = "5th & kennedy st nw"
all_stations[all_stations$station=="4th st & massachusetts ave nw","station"] = "5th st & massachusetts ave nw"
all_stations[all_stations$station=="5th st & k st nw","station"] = "5th & k st nw"
all_stations[all_stations$station=="7th & f st nw / national portrait gallery","station"] = "7th & f st nw/portrait gallery"
all_stations[all_stations$station=="8th & f st nw / national portrait gallery","station"] = "7th & f st nw/portrait gallery"
all_stations[all_stations$station=="8th & f st nw","station"] = "7th & f st nw/portrait gallery"

all_stations[all_stations$station=="11th & k st nw","station"] = "10th & k st nw"
all_stations[all_stations$station=="13th & u st nw","station"] = "12th & u st nw"
all_stations[all_stations$station=="15th & hayes st","station"] = "18th & hayes st"
all_stations[all_stations$station=="bethesda ave & arlington blvd","station"] = "bethesda ave & arlington rd"
all_stations[all_stations$station=="court house metro / wilson blvd & n uhle st","station"] = "court house metro / 15th & n uhle st"
all_stations[all_stations$station=="connecticut ave & nebraska ave nw","station"] = "connecticut & nebraska ave nw"

all_stations[all_stations$station=="fallsgove dr & w montgomery ave","station"] = "fallsgrove dr & w montgomery ave"
all_stations[all_stations$station=="king st metro","station"] = "king st metro south"
all_stations[all_stations$station=="mcpherson square - 14th & h st nw","station"] = "mcpherson square / 14th & h st nw"
all_stations[all_stations$station=="n adams st & lee hwy","station"] = "lee hwy & n adams st"
all_stations[all_stations$station=="n fillmore st & clarendon blvd","station"] = "clarendon blvd & n fillmore st"
all_stations[all_stations$station=="n highland st & wilson blvd","station"] = "clarendon metro / wilson blvd & n highland st"
all_stations[all_stations$station=="n nelson st & lee hwy","station"] = "lee hwy & n nelson st"

all_stations[all_stations$station=="n quincy st & wilson blvd","station"] = "wilson blvd & n quincy st"
all_stations[all_stations$station=="16th & u st nw","station"] = "new hampshire ave & t st nw"
all_stations[all_stations$station=="new hampshire ave & t st nw [formerly 16th & u st nw]","station"] = "new hampshire ave & t st nw"
all_stations[all_stations$station=="randle circle & minnesota ave ne","station"] = "randle circle & minnesota ave se"
all_stations[all_stations$station=="smithsonian / jefferson dr & 12th st sw","station"] = "smithsonian-national mall / jefferson dr & 12th st sw"

all_stations[all_stations$station=="virginia square","station"] = "virginia square metro / n monroe st & 9th st n"
all_stations[all_stations$station=="7th & water st sw / sw waterfront","station"] = "6th & water st sw / sw waterfront"
all_stations[all_stations$station=="wisconsin ave & macomb st nw","station"] = "wisconsin ave & newark st nw"
all_stations[all_stations$station=="wilson blvd & n oakland st","station"] = "virginia square metro / n monroe st & 9th st n"
all_stations[all_stations$station=="mlk library/9th & g st nw","station"] = "10th & g st nw"
all_stations[all_stations$station=="6th & water st sw / sw waterfront","station"] = "4th & m st sw"
all_stations[all_stations$station=="mcpherson square / 14th & h st nw","station"] = "15th & k st nw"
all_stations[all_stations$station=="lee hwy & n nelson st","station"] = "lee hwy & n monroe st"
all_stations[all_stations$station=="1st & n st  se","station"] = "1st & n st se"
all_stations[all_stations$station=="idaho ave & newark st nw [on 2nd district patio]","station"] = "wisconsin ave & newark st nw"
all_stations[all_stations$station=="18th & hayes st","station"] = "aurora hills community ctr/18th & hayes st"


current_stations[current_stations$name=="1st & n st  se","name"] = "1st & n st se"
current_stations[current_stations$name=="eads & 22nd st s","name"] = "22nd & eads st"

# Search for stations in current station list
 #current_stations[grep("newark",current_stations$name),"name"]


# How many of the 504 total stations in CaBi Historical data can be matched to current list of 485 stations?
all_stations = all_stations %>% 
  left_join(current_stations, by=c("station"="name")) %>% 
  mutate(match_ind = ifelse(!is.na(short_name),1,0)) %>% 
  select(orig_station, station, match_ind, short_name, lat, lon, region_id) %>% 
  rename(station_id=short_name)

sum(all_stations$match_ind)
  #497

# 7 stations still don't match
View(all_stations[all_stations$match_ind==0,])
nomatch = c(drops,unlist(all_stations[all_stations$match_ind==0,"orig_station"]))
names(nomatch) = NULL


# Delete stations with no location
stations = all_stations[all_stations$match_ind==1,]
stations$match_ind = NULL

# # TEST STATION - 18TH & M ST NW
# test = stations[stations$station=="18th & m st nw",]

### GET CENSUS TRACT ID FOR EACH CABI STATION

library(rgdal)
library(sp)
library(raster)
library(rgeos)

# List of DC area census tracts (from Uber - manual download)
census = fromJSON("Raw_Data/washington_DC_censustracts.json")
tract_id = census$features$properties$MOVEMENT_ID
polys = census$features$geometry$coordinates

polys_mat = list()
for(i in 1:length(polys)){
  polys_mat[[i]] = matrix(as.vector(polys[i][[1]]), nrow=length(as.vector(polys[i][[1]]))/2, ncol=2, byrow=F)
  
}

ps <- lapply(polys_mat, Polygon)

# add id variable
p1 <- lapply(seq_along(ps), function(i) Polygons(list(ps[[i]]), 
                                                 ID = tract_id[i]  ))
# spatial polygons
tracts = SpatialPolygons(p1, proj4string = CRS("+proj=longlat +datum=WGS84") )

#save spatial polygons for mapping
save(tracts, file="Data/census_tracts_sp.rData")

# GET LIST OF NEIGHBORS FOR EACH CENSUS TRACT
library(spdep)
library(sp)
nb <- poly2nb(tracts)
mat <- nb2mat(nb, style="B")
colnames(mat) <- rownames(mat)
save(mat,file="Data/census_tract_neighbors_matrix.rData")

# get centroids for each census tract (for Uber data)
tract_centers <- SpatialPointsDataFrame(coords=gCentroid(tracts, byid=TRUE), 
                                      data=census$features$properties, match.ID=FALSE)
tract_centroids = data.frame(cbind(tract_centers@data, tract_centers@coords))
tract_centroids$MOVEMENT_ID = as.numeric(tract_centroids$MOVEMENT_ID)
save(tract_centroids, file="Data/census_tract_centroids.rData")

# spatial polygons data frame
tracts_df = SpatialPolygonsDataFrame(tracts, 
                                     data.frame(id = tract_id, 
                                                row.names = tract_id))

#save spatial polygons data frame for mapping
save(tracts_df, file="Data/census_tracts_spdf.rData")

# CaBi stations as spatial points
cabi_sp <- SpatialPointsDataFrame(coords=stations[, c("lon", "lat")],
                                  data=stations[, c("station_id")],
                                  proj4string=CRS("+proj=longlat +datum=WGS84"))


#Spatial overlay to identify census polygon in which each CaBi station point falls
#The Result `cabi_tract` is a dataframe with the tract data for each point
cabi_tract <- cbind(stations, over(x=cabi_sp, y=tracts_df))
cabi_tract = cabi_tract %>% rename(tract=id)

length(unlist(unique(cabi_tract$tract))) #CaBi stations are located in 184 of the 558 DC area census tracts

# Some CaBi stations are outside of the bounds of Uber data (e.g. Montgomery County MD -- Rockville, Shady Grove, Reston VA)
sum(is.na(cabi_tract$tract))
View(cabi_tract[is.na(cabi_tract$tract),])

# Check census tract of two stations near Dupont Circle (data verification)
cabi_tract[cabi_tract$station=="18th & m st nw",]
cabi_tract[cabi_tract$station=="19th & k st nw",]
  #Should be census tract 203

#save(cabi_tract, file="Raw_Data/cabi_tract.rData")

detach(package:raster, unload = TRUE)
library(dplyr)

# Merge lat/long onto full data frame
cabi_frame$start_st = trimws(tolower(as.character(cabi_frame$start_st)))
cabi_frame$end_st = trimws(tolower(as.character(cabi_frame$end_st)))


cabi_frame = cabi_frame %>% 
  left_join(cabi_tract, by=c("start_st"="orig_station")) %>% 
  select(-station) %>% 
  rename(start_st_id=station_id, start_lat=lat, start_lon=lon, start_region_id=region_id, start_tract=tract) %>% 
  left_join(cabi_tract, by=c("end_st"="orig_station")) %>% 
  select(-station) %>% 
  rename(end_st_id=station_id, end_lat=lat, end_lon=lon, end_region_id=region_id, end_tract=tract)


# Check null values
colSums(is.na(cabi_frame[,2:length(cabi_frame)])) #13,875 null start station IDs and 36,867 null census tracts
#table(cabi_frame[is.na(cabi_frame$start_st_id),"start_st"])

# Convert column data types
cabi_frame$start_st = as.factor(cabi_frame$start_st)
cabi_frame$end_st = as.factor(cabi_frame$end_st)
cabi_frame$start_st_id = as.integer(cabi_frame$start_st_id)
cabi_frame$end_st_id = as.integer(cabi_frame$end_st_id)
cabi_frame$start_tract = as.integer(as.character(cabi_frame$start_tract))
cabi_frame$end_tract = as.integer(as.character(cabi_frame$end_tract))


# Calculate distance between CaBi stations using lat-long
pairs = cabi_frame %>% group_by(start_st_id, end_st_id) %>% summarize(start_lon=max(start_lon),
                                                                start_lat=max(start_lat),
                                                                end_lon=max(end_lon),
                                                                end_lat=max(end_lat)) %>% 
  filter(!is.na(start_lon) & !is.na(end_lon)) %>% setDT()

library(geosphere)
pairs = pairs[ , dist_km := distGeo(matrix(c(start_lon, start_lat), ncol = 2), 
                                  matrix(c(end_lon, end_lat), ncol = 2))/1000]

pairs = pairs %>% select(start_st_id, end_st_id, dist_km)

cabi_frame = cabi_frame %>% 
  left_join(pairs, by=c("start_st_id", "end_st_id"))

quantile(pairs$dist_km) # 50% of rides are <= 4.1km distance

# Delete rides with missing station geo coordinates or missing census tracts

cabi = cabi_frame[!is.na(cabi_frame$start_st_id) & 
                    !is.na(cabi_frame$end_st_id) &
                    !is.na(cabi_frame$start_tract) &
                    !is.na(cabi_frame$end_tract) &
                    !is.na(cabi_frame$dist_km),]
rm(cabi_frame)


# Save down
save(cabi, file="Data/cabi_geo.Rdata")
save(cabi_tract, file="Data/cabi_stations_geo.Rdata")

