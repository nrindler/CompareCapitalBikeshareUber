# CABI REGRESSION

#setwd("C:\\Users\\nadav.rindler\\OneDrive - American Red Cross\\Training\\UW Data Science Certificate\\DS450\\Capstone_Project")

library(dplyr)
library(ggplot2)
library(lubridate)

# Import CaBi data
load("Data/cabi_geo.Rdata")

load("Raw_Data/DC_weather_hourly.rData")
weather_hr = weather
weather_hr[is.na(weather_hr$Precip),"Precip"] = 0

load("Raw_Data/DC_weather_daily.rData")
weather[is.na(weather$Snow.Ice.Depth),"Snow.Ice.Depth"] = 0

# Convert Cabi date field to separate Date and Hour fields
cabi$date = as.Date(cabi$start_dt, tz='')
#cabi$date = as.Date(as.character(cabi$start_dt))


# Identify outliers -- trip time > 2 hours
length(cabi[cabi$duration>120,"duration"])/nrow(cabi)
  #1.2% of trips (99th percentile)

# SUBSET CABI DATA
  # Remove trips starting and ending at same station
  # Exclude rides by casual users
  # Exclude rides >120 minutes
  # Exclude weekend rides (longer + with higher variability)
  # DATE RANGE:
      # TRAIN - 1/1/2015 - 12/31/2015
      # TEST - 1/1/2016 - 12/31/2016
      # VALIDATION - 1/1/2017 - 3/31/2017
cabi = cabi[cabi$start_st != cabi$end_st & 
              cabi$member_type == "Member" & 
              cabi$duration<=120 &
              wday(cabi$start_dt)>1 & wday(cabi$start_dt)<7 &
              cabi$date>=as.Date("2015-01-01"),]

cabi$hour = as.numeric(format(cabi$start_dt, "%H"))

# Remove Holidays
holidays = as.Date(c("2015-01-01",
                     "2015-01-19",
                     "2015-02-16",
                     "2015-05-25",
                     "2015-07-03",
                     "2015-09-07",
                     "2015-11-26",
                     "2015-12-25",
                     "2016-01-01",
                     "2016-01-18",
                     "2016-02-15",
                     "2016-05-30",
                     "2016-07-04",
                     "2016-09-05",
                     "2016-11-24",
                     "2016-12-26",
                     "2017-01-02",
                     "2017-01-16",
                     "2017-02-20",
                     "2017-05-29",
                     "2017-07-04",
                     "2017-09-04",
                     "2017-11-23",
                     "2017-12-25"))

cabi = cabi[-which(cabi$date %in% holidays),]

# Save down
# save(cabi,file="Data/cabi_reg.rData")

# SKINNY CABI DATA - Remove unnecessary fields
cabi = cabi %>% 
  select(-c(bike, member_type, start_lat, start_lon, start_tract,
            end_lat, end_lon, end_tract, start_st, start_dt, end_st, end_dt))

# Merge on daily weather data
cabi = cabi %>% 
  left_join(weather, by=c("date"="Date"))

# Merge on hourly weather data
cabi = cabi %>% 
  left_join(weather_hr, by=c("date"="Date", "hour"="Hour"))



# Create time series attributes
cabi$day_count = as.numeric(cabi$date - min(cabi$date))
cabi$hour_fac = as.factor(cabi$hour)
# cabi$week_count = floor(cabi$day_count/7.0)
# cabi$month_count = floor(cabi$day_count/30.5)
# cabi$year_count = as.numeric(format(cabi$date, format="%Y")) - as.numeric(format(min(cabi$date), format="%Y"))

cabi$month = as.numeric(format(cabi$date, format="%m"))
cabi$month_fac = as.factor(cabi$month)
cabi$season = floor((cabi$month-1) / 3)
cabi$season_fac = as.factor(cabi$season)
cabi$weekday = as.numeric(format(cabi$date, format = "%w"))
cabi$weekday_fac = as.factor(cabi$weekday)
cabi$week = as.numeric(format(cabi$date, format = "%W"))
cabi$year = as.numeric(format(cabi$date, format="%Y"))

# Convert duration from difftime to numeric
cabi$duration = as.numeric(cabi$duration)


# Check for null values
colSums(is.na(cabi[,2:length(cabi)]))

# Region ID - convert to indicator for DC vs. non-DC
# 42 - DC
# 41 - VA / Arlington
# 40 - VA / Alexandria
# 44 - MD 
# 104 - VA / Tysons Corner
cabi$start_dc_ind = ifelse(cabi$start_region_id==42,1,0)
cabi$end_dc_ind = ifelse(cabi$end_region_id==42,1,0)

# Add indicator for DC Cherry Blossom Festival each March/April
cabi$cherry_blossom = ifelse(cabi$date>=as.Date("2015-03-20") &
                               cabi$date<=as.Date("2015-04-12"),1,
                      ifelse(cabi$date>=as.Date("2016-03-20") &
                               cabi$date<=as.Date("2016-04-17"),1,
                      ifelse(cabi$date>=as.Date("2017-03-16") &
                               cabi$date<=as.Date("2017-04-15"),1,0)))

# Avg trip time fluctuates by year
mean(cabi[cabi$year==2015,"duration"]) # 11.19
mean(cabi[cabi$year==2016,"duration"]) # 11.58 (23 seconds slower than 2015)
mean(cabi[cabi$year==2017,"duration"]) # 10.84 (44 seconds faster than 2016 as of Mar 2017)

# SPLIT DATA
  # TRAIN - 1/1/2015 - 12/31/2015
  # TEST - 1/1/2016 - 12/31/2016
  # VALIDATION - 1/1/2017 - 3/31/2017
train = cabi[cabi$date<as.Date("2016-01-01"),]
test = cabi[cabi$date>=as.Date("2016-01-01") & cabi$date<as.Date("2017-01-01"),]
validation = cabi[cabi$date>=as.Date("2017-01-01"),]


### DATA EXPLORATION

# Average trip time by day. look for outliers
cabi_day = cabi %>% group_by(date) %>% summarize(avg_trip_time=mean(duration))
ggplot(cabi_day, aes(x=date, y=avg_trip_time)) +
  geom_bar(stat="identity", fill="dark blue") +
  labs(title="Average Daily CaBi Trip Time", subtitle="Jan 2015 - Mar 2017",
       x = "Date", y = "Average Trip Time (Minutes)")
quantile(cabi_day$avg_trip_time)
# 5 days in late Feb 2016 with missing data
# most days average trip time is between [10,12.5]. range [8,15]

# Average trip time by day of week
cabi_dow = cabi %>% group_by(weekday) %>% summarize(avg_trip_time=mean(duration))
ggplot(cabi_dow, aes(x=weekday, y=avg_trip_time)) +
  geom_bar(stat="identity", fill="dark blue") +
  labs(title="Average CaBi Trip Time by Day of Week", subtitle="Jan 2015 - Mar 2017",
       x = "Date", y = "Average Trip Time (Minutes)")
    # weekdays are about the same, weekends are longer

# Average trip time by day of week
cabi_hod = cabi %>% group_by(hour) %>% summarize(avg_trip_time=mean(duration))
ggplot(cabi_hod, aes(x=hour, y=avg_trip_time)) +
  geom_bar(stat="identity", fill="dark blue") +
  labs(title="Average CaBi Trip Time by Hour of Day", subtitle="Jan 2015 - Mar 2017",
       x = "Date", y = "Average Trip Time (Minutes)")
  # longer trips during AM and PM peak hours, shortest trips during night


# Assess multicollinearity between daily/hourly weather attributes
library(corrplot)
corr = cor(cabi[,c(10:20,22:24,26:28)])
corrplot(corr, method="square", type="lower")

# Delete factor vars not used in regression
train[,c("WindDir","Conditions")] = NULL
test[,c("WindDir","Conditions")] = NULL
validation[,c("WindDir","Conditions")] = NULL

# Linear Regression to Predict Trip Duration
reg = lm(duration ~ . - start_st_id
        - end_st_id
        - date
        - start_region_id 
        - end_region_id
         -year 
         - CloudFrac 
         - day_count
        -month - season - season_fac - weekday_fac - week - hour
        -Avg.Temp - HDDs - CDDs - Dewpoint - Precip - Snow.Ice.Depth 
        - RelHumid - Pressure - Precipitation.Water.Equiv - Snowfall - Max.Temp
        - cherry_blossom, data=train)
summary(reg)

# 4.9 mins per km (Slow! only 12.2km/hr)

# Predict on test data
test$reg_pred = predict(reg, newdata=test)

# Calculate MAE
mean(abs(test$duration - test$reg_pred)) # 2.741804 minutes

# Calculate MAPE
mean(abs(test$duration - test$reg_pred))/mean(test$duration) # 23.68559%



# Evaluate performance on validation set
validation$reg_pred = predict(reg, newdata=validation)

# Calculate MAE
mean(abs(validation$duration - validation$reg_pred)) # 2.453838 minutes

# Calculate MAPE
mean(abs(validation$duration - validation$reg_pred))/mean(validation$duration) # 22.63996%

write.csv(reg$coefficients, file="Figure/regression_coeffs.csv")





