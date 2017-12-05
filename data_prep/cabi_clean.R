#-----------------------------
#
#  Capital Bikeshare (CaBi) Ride Data
#
#  Purpose: Clean and combine raw CaBi data
#
#  Created by: Nadav Rindler 
#
#  Created on: 2017-10-17
#
#-----------------------------

#setwd("C:\\Users\\nadav.rindler\\OneDrive - American Red Cross\\Training\\UW Data Science Certificate\\DS450\\Capstone_Project")

detach(package:raster, unload = TRUE)
library(dplyr)
require(RSQLite)
require(zoo)
require(stringr)

# Run scraper function to download raw CaBi data from 2010-Q4 to 2017-Q1
# source("Data_Prep/cabi_retrieval.R")

load("Raw_Data/cabi_raw.Rdata")

yq = format(as.yearqtr(seq(from=as.Date('2010-10-01'),to=as.Date('2017-01-01'),by='quarter')),'%Y-Q%q')
yq2 = format(as.yearqtr(yq, format='%Y-Q%q'), '%Y%q')
yq = c(yq,"2016-Q3")
yq2 = c(yq2,"201632")

# Rename data frame
cabi = cabi_list
rm(cabi_list)
names(cabi) = yq

# Drop duration field for all quarters
for(i in 1:length(cabi)){
  cabi[[i]][1] = NULL
}


# Get names of columns for each quarter's data set
for(i in 6:27){ #length(cabi)
  print(i)
  print(names(cabi[i]))
  print(names(cabi[[i]]))
  print("")
}

# # Get list of station names & station IDs (only in certain quarters)
# stations = cabi[c(1:5,20:27)]
# for(i in 1:length(stations)){
#   if(i<=5){
#     names(stations[[i]]) = c("start_dt","end_dt","start_st","end_st","bike","member_type")
#     stations[[i]]$start_st_id = str_sub(stations[[i]]$start_st,-6,-2)
#     stations[[i]]$end_st_id = str_sub(stations[[i]]$end_st,-6,-2)
#     stations[[i]] = stations[[i]] %>% select(start_st, start_st_id, end_st, end_st_id)
#   } else {
#     names(stations[[i]]) = c("start_dt","end_dt","start_st_id","start_st","end_st_id","end_st","bike","member_type")
#     stations[[i]] = stations[[i]] %>% select(start_st, start_st_id, end_st, end_st_id)
#   }
# }
# 
# # Concatenate DFs together
# station_frame = stations[[1]]
# for(i in 2:length(stations)){
#   station_frame = rbind(station_frame, stations[[i]])
# }
# rm(stations)
# 
# # List unique stations and station IDs
# names(station_frame) = c("station","station_id","station","station_id")
# stations=rbind(station_frame[,1:2], station_frame[,3:4])
# rm(station_frame)
# 
# stations = stations %>%
#   select(station, station_id) %>%
#   mutate(station = trimws(tolower(station), which = "both"),
#          station_id = trimws(tolower(station_id), which = "both")) %>% 
#   group_by(station, station_id) %>%
#   unique()
# 
# # clean up station names
# stations$station_clean =  as.character(lapply(strsplit(stations$station, '[()]'), function(x) x[1]))
# stations = stations %>%
#   select(station_clean, station_id) %>%
#   unique() %>% 
#   rename(station=station_clean)
#   
# save(stations, file="Data/cabi_stations.Rdata")


# Rename all fields with consistent names
for(i in 1:length(cabi)){
  if(i<6){
    names(cabi[[i]]) = c("start_dt","end_dt","start_st","end_st","bike","member_type")
    cabi[[i]]$start_st = str_sub(cabi[[i]]$start_st,1,-8)
    cabi[[i]]$end_st = str_sub(cabi[[i]]$end_st,1,-8)
  } else if(i<13){
    names(cabi[[i]]) = c("start_dt","end_dt","start_st","end_st","bike","member_type")
  } else if(i<20){
    names(cabi[[i]]) = c("start_dt","start_st","end_dt","end_st","bike","member_type")
    cabi[[i]] = cabi[[i]] %>% select(start_dt, end_dt, start_st, end_st, bike, member_type)
  } else if(i<=27){
  names(cabi[[i]]) = c("start_dt","end_dt","start_st_id","start_st","end_st_id","end_st","bike","member_type")
  cabi[[i]] = cabi[[i]] %>% select(start_dt, end_dt, start_st, end_st, bike, member_type)
  }
}

# Change date formats
    # !!! IF REDOING, PRESERVE TRIP DURATION SECONDS FROM "Duration" FIELD !!!
for(i in 1:length(cabi)){
  # Different date format for 2014-Q4 file
  if(i==17){
  cabi[[i]] = cabi[[i]] %>% 
    mutate(start_dt = as.POSIXct(start_dt, format= "%Y-%m-%d %H:%M"),
       end_dt = as.POSIXct(end_dt, format= "%Y-%m-%d %H:%M"),
       duration = difftime(end_dt, start_dt, unit = "min"))
  } else{
    cabi[[i]] = cabi[[i]] %>% 
      mutate(start_dt = as.POSIXct(start_dt, format= "%m/%d/%Y %H:%M"),
             end_dt = as.POSIXct(end_dt, format= "%m/%d/%Y %H:%M"),
             duration = difftime(end_dt, start_dt, unit = "min"))
  }
}

# Concatenate DFs together
cabi_frame = cabi[[1]]
for(i in 2:length(cabi)){
  cabi_frame = rbind(cabi_frame, cabi[[i]])
}


# Data cleaning

# How many unique values for each variable?
length(unique(cabi_frame$bike)) #4,688
length(unique(cabi_frame$start_st)) #647
length(unique(cabi_frame$end_st)) #648
unique(cabi_frame$member_type)

quantile(cabi_frame$duration, na.rm=T)

# Set member type to either Casual or Member
cabi_frame$member_type = ifelse(cabi_frame$member_type=="Casual","Casual","Member")

# Delete observations with negative trip duration 
# length(cabi_frame[cabi_frame$duration<0 & !is.na(cabi_frame$duration),"duration"]) #89
cabi_frame = cabi_frame[cabi_frame$duration>=0 | is.na(cabi_frame$duration),]

# View missing data
colSums(is.na(cabi_frame[,1:7])) #none

# View empty char fields
sum(cabi_frame$start_st=="") #0
sum(cabi_frame$end_st=="") #72
sum(cabi_frame$bike=="") #54
sum(cabi_frame$member_type=="") #0

# Delete rows with missing end station
cabi_frame = cabi_frame[cabi_frame$end_st!="",]

# Trim whitespace for station names
cabi_frame$start_st = trimws(tolower(cabi_frame$start_st), which = "both")
cabi_frame$end_st = trimws(tolower(cabi_frame$end_st), which = "both")


# Convert character vars to factor
cabi_frame$start_st = as.factor(cabi_frame$start_st)
cabi_frame$end_st = as.factor(cabi_frame$end_st)
cabi_frame$bike = as.factor(cabi_frame$bike)
cabi_frame$member_type = as.factor(cabi_frame$member_type)

cabi_frame$duration = as.numeric(cabi_frame$duration)

# Data check: The set of starting stations and ending stations are the same in historical CaBi data
all_start_stations = data.frame(station=levels(cabi_frame$start_st))
all_end_stations = data.frame(station=levels(cabi_frame$end_st))

all_stations_overlap = all_start_stations %>%
  inner_join(all_end_stations, by="station")


#Save down
save(cabi_frame,file="Data/cabi.Rdata")
rm(cabi)

# # Write to SQLite DB
# sql_db_name = 'Data/cabi.sqlite'
# con = dbConnect(SQLite(), dbname=sql_db_name)
# 
# dbWriteTable(con, name="cabi", cabi_frame, overwrite=TRUE)
# 
# dbDisconnect(con)
# # WAY TOO BIG IN SQL! 1.3 GB vs. 162 MB in Rdata format
