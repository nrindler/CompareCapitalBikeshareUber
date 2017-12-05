#-----------------------------
#
#  Capital Bikeshare (CaBi) vs. Uber Analysis
#
#  Purpose: Runs all scrips associated with this analysis
#
#  Created by: Nadav Rindler 
#
#  Created on: 2017-12-03
#
#-----------------------------

# set working directory
setwd("C:\\Users\\nadav.rindler\\OneDrive - American Red Cross\\Training\\UW Data Science Certificate\\DS450\\Capstone_Project")

# import required packages
#import package list from packages.txt file
packages <- c("RSQLite",
              "logging",
              "stringr",
              "zoo",
              "dplyr",
              "jsonlite",
              "lubridate",
              "data.table",
              "ggplot2",
              "tidyverse",
              "geosphere",
              "rgdal",
              "sp",
              "raster",
              "rgeos",
              "leaflet",
              "htmlwidgets")


#install new packages if needed
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(packages, require, character.only = TRUE)


# run R scripts

# Data preparation

source("Data_Prep/cabi_retrieval.R") # download CaBi quarterly CSV files
    # this script takes about an hour to run
source("Data_Prep/cabi_clean.R") # clean and combine CaBi data sets
    # this script takes 20 minutes to run
source("Data_Prep/cabi_stations.R") # clean CaBi stations and get lat/long coordinates
source("Data_Prep/weather.R") # clean CaBi stations and get lat/long coordinates
# Uber data dowloaded manually from https://movement.uber.com/explore/washington_DC/travel-times/

# Analysis

source("Analysis/cabi_regression.R") # linear regression for CaBi trip duration
source("Analysis/cabi_uber_trips.R") # compare CaBi and Uber ride time
    # the monte carlo simulation takes about six hours to run
source("Analysis/mapping.R") # generate interactive maps with Leaflet









