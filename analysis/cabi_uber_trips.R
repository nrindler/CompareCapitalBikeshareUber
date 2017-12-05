# COMPARISON OF CABI AND UBER TRIP TIMES

#setwd("C:\\Users\\nadav.rindler\\OneDrive - American Red Cross\\Training\\UW Data Science Certificate\\DS450\\Capstone_Project")

library(jsonlite)
library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)
library(data.table)
library(ggplot2)
library(geosphere)

load("Data/cabi_geo.Rdata")

cabi = cabi[cabi$start_st != cabi$end_st & 
              cabi$member_type == "Member" & 
              cabi$duration<=120 &
              wday(cabi$start_dt)>1 & wday(cabi$start_dt)<7,]
              #cabi$date>=as.Date("2015-01-01")

# Fix duration field - convert to seconds
cabi$duration = difftime(cabi$end_dt, cabi$start_dt, units="secs")
cabi$duration = as.numeric(cabi$duration)            

# Extract date and hour from start timestamp
cabi$date = as.Date(cabi$start_dt)
cabi$hour = hour(cabi$start_dt)

# SKINNY CABI DATA - Remove unnecessary fields
cabi = cabi %>% 
  select(-c(bike, member_type, start_dt, end_dt)) 

# Delete trips starting and ending in same census tract
cabi = cabi[cabi$start_tract!=cabi$end_tract,]

# Group CaBi rides by time of day
cabi$time = ifelse(cabi$hour %in% 8:10, "1AM",
                   ifelse(cabi$hour %in% 11:15, "2MD",
                          ifelse(cabi$hour %in% 16:19, "3PM",
                                 ifelse(cabi$hour %in% 20:23, "4EV",
                                        "5NI"))))
cabi$time = as.factor(cabi$time)


# Must have minimum 100 trip originations in each start tract

# 171 of 181 census tracts have at least 100 CaBi trip originations
cabi_tract = cabi %>% group_by(start_tract) %>% summarize(nobs=n())
quantile(cabi_tract$nobs)
nrow(cabi_tract[cabi_tract$nobs<100,])

drop = unlist(cabi_tract[cabi_tract$nobs<100,"start_tract"])

cabi = cabi[-which(cabi$start_tract %in% drop),]

# Must have minimum 5 rides between any two census tract for each time bucket
    # in order to make comparison

# Number of observations per trip (start/end census tract) and time bucket 
combos = cabi %>% group_by(start_tract, end_tract, time) %>% summarize(nobs=n())

#trips with <5 observations
nrow(combos[combos$nobs<5,])/nrow(combos) # 36% of trip/time bucket combinations

drop = cabi_tract[cabi_tract$nobs<100,"start_tract"]

cabi = cabi %>%
  inner_join(combos[combos$nobs>=5,], by=c("start_tract","end_tract","time")) %>% 
  select(-nobs)

# Number of observations per trip (start/end census tract)  
trips = cabi %>% group_by(start_tract, end_tract) %>% summarize(nobs=n())

#top 10 trips
head(trips[order(trips$nobs, decreasing=T),],10)

#investigate census tracts for top 10 trips
census = fromJSON("Raw_Data/washington_DC_censustracts.json")
geo = census$features$properties
geo[geo$MOVEMENT_ID %in% c(203, 61, 154, 233, 168, 228, 66, 186, 105),]
# all clustered around downtown DC's business district, as expected


#save(cabi, file="Data/cabi_subset.Rdata")





### UBER ###

# # Import Uber data
# 
# # Loop over filepaths
yq = format(as.yearqtr(seq(from=as.Date('2016-01-01'),to=as.Date('2017-09-01'),by='quarter')),'%Y-%q')
yq2 = format(as.yearqtr(seq(from=as.Date('2016-01-01'),to=as.Date('2017-09-01'),by='quarter')),'%Y%q')
type = c("Monthly","Hourly")
type2 = c("mo","hr")
# 
# fnames = list()
# rnames = list()
# cnt = 1
# for(i in 1:length(yq)){
#   for(k in 1:length(type)){
#     fnames[[cnt]] = paste0("Raw_Data/washington_DC-censustracts-", yq[i], "-OnlyWeekdays-", type[k],
#                            "Aggregate.csv")
#     rnames[[cnt]] = paste("uber",yq2[i], type2[k],   sep="_")
#     cnt = cnt + 1
#   }
# }
# fn = unlist(fnames)
# rn = unlist(rnames)
# 
# uber = list()
# for(i in 1:length(fnames)){
#   uber[[i]] = read.csv(fn[i])
# }
# names(uber) = rn
# 
# save(uber, file="Raw_Data/uber_raw.Rdata")
# 
load("Raw_Data/uber_raw.Rdata")



# Limit Uber to same geo coverage as CaBi

# Unique pairs of start/end census tract for all CaBi rides
pairs = distinct(setDF(cabi), start_tract, end_tract) #8,648 pairs

test = uber[[1]]
pairs_uber1 = distinct(setDF(test), sourceid, dstid) #155,936 pairs
nrow(pairs_uber1)

# Subset Uber to only the census tracts included in CaBi data
for(i in 1:length(uber)){
  uber[[i]] = uber[[i]] %>% inner_join(pairs, by=c("sourceid"="start_tract","dstid"="end_tract"))
}

#Monthly Uber data
mo = names(uber)[grepl("mo", names(uber))]
uber_month = as.data.frame(uber[[mo[1]]])
uber_month$yq = yq[1]
for(i in 2:length(mo)){
  uber[[mo[i]]]$yq = yq[i]
  uber_month = rbind(uber_month, uber[[mo[i]]])
}


#Hourly Uber data
hr = names(uber)[grepl("hr", names(uber))]
uber_hod = as.data.frame(uber[[hr[1]]])
uber_hod$yq = yq[1]
for(i in 2:length(hr)){
  uber[[hr[i]]]$yq = yq[i]
  uber_hod = rbind(uber_hod, uber[[hr[i]]])
}


# Combine Uber travel time distributions
# average across hours of day


# Apply geo centroids for each census tract to Uber data to obtain trip distance

# 181 unique census tracts
tracts = unique(c(unique(pairs$start_tract), unique(pairs$end_tract)))

# Geo centroids for each census tract
load("Data/census_tract_centroids.rData")
tract_centroids = tract_centroids[which(tract_centroids$MOVEMENT_ID %in% tracts),]

uber_pairs = uber_hod %>% 
  group_by(sourceid, dstid) %>% 
  distinct(sourceid, dstid)

uber_pairs = uber_pairs %>% 
  left_join(tract_centroids, by=c("sourceid" = "MOVEMENT_ID")) %>% 
  select(-DISPLAY_NAME) %>% 
  rename(start_lon=x, start_lat=y) %>% 
  left_join(tract_centroids, by=c("dstid" = "MOVEMENT_ID")) %>% 
  select(-DISPLAY_NAME) %>% 
  rename(end_lon=x, end_lat=y)

uber_pairs = as.data.table(uber_pairs)
uber_pairs = uber_pairs[ , dist_km := distGeo(matrix(c(start_lon, start_lat), ncol = 2), 
                                              matrix(c(end_lon, end_lat), ncol = 2))/1000]


#Add uber year-month variable 
uber_month$ym = as.Date(strptime(paste(uber_month$month,"1",substr(uber_month$yq,1,4),sep="/"),
                                 format="%m/%d/%Y"))

uber_month = uber_month %>% 
  left_join(uber_pairs, by=c("sourceid","dstid"))

uber_hod = uber_hod %>% 
  left_join(uber_pairs, by=c("sourceid","dstid"))

quantile(uber_pairs$dist_km) # 50% of trip combinations are <= 3.7km distance


# group hour of day into buckets
# hour_of_day %in% 8:10 ~ "AM Peak",
# hour_of_day %in% 11:15 ~ "Midday",
# hour_of_day %in% 16:18 ~ "PM Peak",
# hour_of_day %in% 19:21 ~ "Evening",
# else "Early Morning"

uber_hod$time = ifelse(uber_hod$hod %in% 8:10, "1AM",
                       ifelse(uber_hod$hod %in% 11:15, "2MD",
                              ifelse(uber_hod$hod %in% 16:19, "3PM",
                                     ifelse(uber_hod$hod %in% 20:23, "4EV",
                                            "5NI"))))
uber_hod$time = as.factor(uber_hod$time)

save(uber_month, file="Data/uber_month.rData")
save(uber_hod, file="Data/uber_hod.rData")


### ANALYSIS

# Evaluate average trip time by ym and hour of day 
mean(cabi$duration/60)

cabi_ym = cabi %>% mutate(ym= as.yearmon(date)) %>% group_by(ym) %>%
  summarize(avg_trip_time = mean(duration))

# Spike Sept 2014
ggplot(cabi_ym, aes(x=ym, y=avg_trip_time)) + geom_bar(stat="identity", fill="dark blue")

cabi_ym$index = as.numeric(row.names(cabi_ym))

# No statistically significant trend over time
reg_ym = lm(avg_trip_time ~ index, data=cabi_ym)
summary(reg_ym)

# Monthly seasonality stable 
reg_ym = lm(avg_trip_time ~ as.factor(month(ym)), data=cabi_ym)
summary(reg_ym)
cabi_ym$pred_reg = predict(reg_ym, newdata=cabi_ym)

ggplot(cabi_ym) +
  geom_bar(aes(x=ym, y=avg_trip_time/60), stat="identity", fill="dark blue") +
  geom_line(aes(x=ym, y=pred_reg/60), size=1, color="red") +
  labs(title="Average Monthly CaBi Ride Time",
       subtitle="2010-2017",
       x="Year", y="Average Trip Time (Minutes)")
ggsave("Figure/cabi_ym.png", width=8, height=5.33, units="in")

# Cabi ride time by hour of day
cabi_hour = cabi %>% group_by(hour) %>% summarize(avg_trip_time=mean(duration/60))
ggplot(cabi_hour) +
  geom_bar(aes(x=hour, y=avg_trip_time), stat="identity", fill="dark blue") +
  labs(title="Average CaBi Ride Time by Hour of Day",
       subtitle="2010-2017",
       x="Hour of Day", y="Average Trip Time (Minutes)")
ggsave("Figure/cabi_hour.png", width=8, height=5.33, units="in")


# Test Uber ride time for the most popular trip
test = uber_hod[uber_hod$sourceid==203 & uber_hod$dstid==61,]

ggplot(test, aes(x=yq, y=mean_travel_time/60, color=hod)) +
  geom_point(size=3)

test$yq2 = as.numeric(as.factor(test$yq))
ggplot(test, aes(x=hod, y=mean_travel_time/60, color=yq2)) +
  geom_point(size=3) +
  labs(title="Average Quarterly Uber Ride Time by Hour of Day",
       subtitle="For Most Popular CaBi trip (Farragut Square - 14th St)",
       x="Hour of Day", y="Average Trip Time (Minutes)",
       color="Quarter")
ggsave("Figure/uber_yq_hod.png", width=8, height=5.33, units="in")

#Cabi start tract with the most rides originating
cabi_start_tract = cabi %>% 
  group_by(start_tract) %>% 
  summarize(nobs=n()) %>% 
  arrange(desc(nobs))
# tract 203 (Farragut Square)










# MONTE CARLO SIMULATION
run_simulation = function(dset,
                         dimensions = c("start_tract","end_tract","time"),
                         min_observations=5,
                         n_samples = 10000) {

  dset = dset[dset$nobs>=min_observations,]
  
  map(1:nrow(dset), function(i) { 

    simulation = dset[i, ]

    simulation_trips = cabi %>%
      inner_join(simulation, by = dimensions) %>% 
      select(time, hour, duration)
      
    weights = simulation_trips %>% 
      group_by(hour) %>% 
      summarize(nobs=n()) %>% 
      mutate(weight=nobs/sum(nobs)) %>% 
      arrange(hour)
    
    uber_simulation = uber_hod %>%
      inner_join(simulation, by = c("sourceid"="start_tract",
                                    "dstid"="end_tract",
                                    "time"="time")) %>%
      filter(yq=="2017-3") %>%  #ONLY TAKE LATEST QUARTER OF UBER DATA FOR COMPARISON
      select(hod, mean_travel_time, standard_deviation_travel_time) %>% 
      inner_join(weights[,c("hour","weight")], by=c("hod"="hour")) %>% 
      arrange(hod)
    
    # COMBINE UBER DISTRIBUTIONS
    uber_wavg = weighted.mean(uber_simulation$mean_travel_time, 
                              w=uber_simulation$weight)
    uber_sd_sim = c()
    for(k in 1:nrow(uber_simulation)){
      uber_sd_sim2 = rnorm(unlist(n_samples*weights[k,"weight"]), 
                           mean=uber_simulation[k,"mean_travel_time"],
                           sd=uber_simulation[k,"standard_deviation_travel_time"])
      uber_sd_sim = c(uber_sd_sim,uber_sd_sim2)
    }
    uber_sd = sd(uber_sd_sim)
    
    # MONTE CARLO SIMULATION
    results = data_frame(
      uber = rnorm(n_samples, mean=uber_wavg, sd=uber_sd),
      cabi = sample(simulation_trips$duration, n_samples, replace = TRUE),
      uber_wins = uber < cabi,
      winning_margin = abs(uber - cabi)
    )

  uber_wins = sum(results$uber_wins)

  winning_margins = results %>%
    group_by(uber_wins) %>%
    summarize(
      avg = mean(winning_margin),
      median = median(winning_margin)
    ) %>%
    ungroup()

  simulation %>%
    mutate(
      uber_median = uber_wavg,
      uber_mean = uber_wavg,
      uber_sd = uber_sd,
      cabi_median = median(simulation_trips$duration),
      cabi_mean = mean(simulation_trips$duration),
      cabi_sd = sd(simulation_trips$duration),
      uber_win_rate = uber_wins / n_samples,
      uber_avg_winning_margin = ifelse(uber_wins == 0, NA, filter(winning_margins, uber_wins)$avg),
      cabi_avg_winning_margin = ifelse(uber_wins == n_samples, NA, filter(winning_margins, !uber_wins)$avg),
      cabi_percentile = pnorm(cabi_mean, mean=uber_mean, sd=uber_sd, lower.tail=F)
    )

}) %>% bind_rows()
}

# !!! WARNING - TAKES 6 HOURS OF PROCESSING TIME!
# set.seed(1234)
# simulation_results = run_simulation(combos)
# save(simulation_results, file="Data/simulation_results.rData")
# 
# # check for missing data
# colSums(is.na(simulation_results[,1:length(simulation_results)]))
# 
# # 10 obs with missing Uber data
# uber_combos = uber_hod %>% 
#   distinct(sourceid, dstid, time) %>%
#   mutate(exists=1)
# 
# nulls = simulation_results[is.na(simulation_results$uber_median),] %>% 
#   left_join(combos, by=dimensions) %>% 
#   left_join(uber_combos, by=c("start_tract"="sourceid", "end_tract"="dstid","time"="time")) %>% 
#   rename(nobs=nobs.x)
# View(nulls)
# 
# # 6 of them have corresponding uber data
#   # retry simulation
# try2 = run_simulation(nulls[!is.na(nulls$exists), c(dimensions,"nobs")])
# try2
# 
# #Didn't work -- delete nulls
# simulation_results = simulation_results[!is.na(simulation_results$uber_median),]
# 
# save(simulation_results, file="Data/simulation_results.rData")

load("Data/simulation_results.rData")

# Get weighted average Uber win rate (weighted by # CaBi rides per bucket)
1 - weighted.mean(simulation_results$uber_win_rate, simulation_results$nobs)
  # CaBi rides are faster than 35.8% of corresponding Uber trips on average

# CaBi rides are faster than 35.8% of corresponding Uber trips on average
simulation_results %>% 
  filter(time %in% c("1AM","3PM")) %>% 
  group_by() %>% 
  summarize(wavg = 1 - weighted.mean(uber_win_rate, nobs))
# CaBi rides are faster than 40.1% of corresponding Uber trips on average

# Compare by time bucket
simulation_results %>% 
  group_by(time) %>% 
  summarize(wavg = 1 - weighted.mean(uber_win_rate, nobs))
  # Cabi win rate from 26.6% in evening hours to 42.5% during AM peak commute hours


### ONE-TO-ONE COMPARISON FOR EACH CABI RIDE - GET PERCENTILE VS. UBER DISTRIBUTION

# For each CaBi ride, get percentile vs. Uber rides (Uber 2017 Q3 data only)
cabi$yq = paste(year(cabi$date),quarter(cabi$date), sep="-")
cabi = cabi %>% 
  left_join(uber_hod[uber_hod$yq=="2017-3",c("sourceid","dstid","hod","mean_travel_time","standard_deviation_travel_time")], 
            by=c("start_tract"="sourceid", "end_tract"="dstid", "hour"="hod"))

cabi$cabi_win_pct = pnorm(cabi$duration, mean=cabi$mean_travel_time,
                          sd=cabi$standard_deviation_travel_time, lower.tail=F)
cabi$cabi_win_margin_uber_avg = cabi$mean_travel_time - cabi$duration


# CaBi rides are faster than 35.5% of corresponding Uber trips on average
mean(cabi$cabi_win_pct, na.rm=T)

# CaBi rides are faster than 39.8% of corresponding Uber trips on average
mean(cabi[which(cabi$time %in% c("1AM","3PM")),"cabi_win_pct"], na.rm=T)

quantile(cabi$cabi_win_pct, na.rm=T)

nrow(cabi[cabi$cabi_win_pct>=0.5,])/nrow(cabi) 
    #33% of Cabi rides beat Uber rides >=50% of the time
nrow(cabi[cabi$cabi_win_pct>=0.9,])/nrow(cabi) 
    #1% of Cabi rides beat Uber rides >=90% of the time

nrow(cabi[which(cabi$time %in% c("1AM","3PM")) & cabi$cabi_win_pct>=0.5,])/nrow(cabi[which(cabi$time %in% c("1AM","3PM")),]) 
    #58% of Cabi peak hour rides beat Uber rides >=50% of the time
nrow(cabi[which(cabi$time %in% c("1AM","3PM")) & cabi$cabi_win_pct>=0.9,])/nrow(cabi[which(cabi$time %in% c("1AM","3PM")),]) 
    #2% of Cabi peak hour rides beat Uber rides >=90% of the time



# Compare by hod - ALL DATA
cabi_win_rate_hour = cabi %>% 
  filter(!is.na(cabi_win_pct)) %>% 
  group_by(hour) %>% 
  summarize(avg_cabi_win_rate = mean(cabi_win_pct))
cabi_win_rate_hour

ggplot(cabi_win_rate_hour, aes(x=hour, y=avg_cabi_win_rate)) +
  geom_bar(stat="identity", fill="dark blue") +
  labs(title="When Capital Bikeshare is faster than Uber",
       subtitle="% of weekday CaBi rides within CaBi service area expected to be faster than equivalent Uber ride",
       x="Hour of Day",
       y="CaBi Win Rate")
ggsave("Figure/cabi_win_rate_all.png", width=8, height=5.33, units="in")

# Compare by hod - MOST POPULAR CABI TRIP
cabi_win_rate_hour_203_61 = cabi %>% 
  filter(!is.na(cabi_win_pct) & start_tract==203 & end_tract==61) %>% 
  group_by(hour) %>% 
  summarize(avg_cabi_win_rate = mean(cabi_win_pct))
cabi_win_rate_hour_203_61

ggplot(cabi_win_rate_hour_203_61, aes(x=hour, y=avg_cabi_win_rate)) +
  geom_bar(stat="identity", fill="dark blue") +
  labs(title="CaBi vs. Uber, Farragut Square to 14th St.",
       subtitle="% of weekday CaBi rides between Farragut & 14th St expected to be faster than equivalent Uber ride",
       x="Hour of Day",
       y="CaBi Win Rate")
ggsave("Figure/cabi_win_rate_203_61.png", width=8, height=5.33, units="in")

# Compare by trip distance in KM
cabi_win_rate_dist = cabi %>% 
  filter(!is.na(cabi_win_pct)) %>% 
  mutate(dist=round(dist_km)) %>% 
  group_by(dist) %>% 
  summarize(avg_cabi_win_rate = mean(cabi_win_pct))
cabi_win_rate_dist

ggplot(cabi_win_rate_dist, aes(x=dist, y=avg_cabi_win_rate)) +
  geom_bar(stat="identity", fill="dark blue") +
  labs(title="CaBi vs. Uber by Trip Distance",
       subtitle="% of weekday CaBi rides within CaBi service area expected to be faster than equivalent Uber ride",
       x="Trip Distance (km)",
       y="CaBi Win Rate")
ggsave("Figure/cabi_win_rate_dist_km.png", width=8, height=5.33, units="in")

# Uber regression
dc = cabi %>% distinct(start_tract, start_region_id) 
uber_hod = uber_hod %>% left_join(dc, by=c("sourceid"="start_tract")) %>% 
  mutate(dc = ifelse(start_region_id==42,1,0))
uber_hod$year = as.numeric(substr(uber_hod$yq,1,4))
uber_hod$season = as.factor(substr(uber_hod$yq,6,6))
uber_hod$quarter = as.numeric(as.factor(uber_hod$yq))

uber_reg = lm(mean_travel_time/60 ~ as.factor(hod) + dist_km + dc +  season + quarter,
              data=uber_hod)
summary(uber_reg)

write.csv(uber_reg$coefficients, file="Figure/uber_reg_coefficients.csv")



