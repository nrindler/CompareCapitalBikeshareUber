setwd("C:\\Users\\nadav.rindler\\OneDrive - American Red Cross\\Training\\UW Data Science Certificate\\DS450\\Capstone_Project")

# Get DAILY historical DC weather data
url = "http://www.frontierweather.com/historicaldataonly/KDCA_daily.txt"
weather = read.csv(url)
weather$Site4 = NULL
weather$Source = NULL
weather$Date = as.Date(as.POSIXct(weather$Date, format="%m/%d/%Y %H:%M:%S"))

#Subset to 2010 onwards
weather = weather[weather$Date>=as.Date("2010-01-01"),]

save(weather, file="Raw_Data/DC_weather_daily.rData")



# Get HOURLY historical DC weather data
url = "http://www.frontierweather.com/historicaldataonly/KDCA.txt"
weather = read.csv(url)
weather$Site = NULL
weather$Source = NULL
names(weather) = c("Date","Hour","Temperature","Dewpoint","RelHumid",
                   "WindDir","WindSpeed","CloudFrac","Pressure","Conditions",
                   "Precip")
weather$Date2 = as.POSIXct(weather$Date, format="%m/%d/%Y %H:%M:%S")
weather$Date = as.Date(weather$Date2)
weather$Date2 = NULL

#Subset to 2010 onwards
weather = weather[weather$Date>=as.Date("2010-01-01"),]

#Remove non-occurring factor levels
weather$Conditions = as.factor(as.character(weather$Conditions))

# Create Indicators for Rain and Snow based on METAR weather codes
# https://en.wikipedia.org/wiki/METAR
rain = c("TS","RA","DZ","UP")
snow = c("SN","SG","IC","IP","GR","GS")

weather$rain = ifelse(grepl(paste(rain, collapse="|"), weather$Conditions)==T,1,0)
weather$snow = ifelse(grepl(paste(snow, collapse="|"), weather$Conditions)==T,1,0)
weather$obscure = NA

save(weather, file="Raw_Data/DC_weather_hourly.rData")

