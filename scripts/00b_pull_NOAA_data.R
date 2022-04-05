## ---------------------------
## Script name: Pull NOAA weather data
## Author: Grace Kuiper
## Version: 09/23/2021

## Purpose: Pull NOAA temperature and RH data using `rnoaa` package. Clean data to 
##          create daily dataset to merge with PM2.5 covariate data.

## Email: grace.kuiper@colostate.edu
## ---------------------------

library(tidyverse)
library(data.table)
library(lubridate)
library(eeptools)
library(dplyr)
library(readxl)
library(rnoaa)
library(riem)

source("riem_helper_functions.R")

####Pull weather data from ASOS stations####
#'Locate nearby stations
AK_ASOS_networks <- c("AK_ASOS")
ASOS_stations <- data.frame()
for (i in 1:length(AK_ASOS_networks)) {
  temp_stations <- riem_stations(AK_ASOS_networks[i]) %>%
    mutate(network=AK_ASOS_networks[i])
  ASOS_stations <- bind_rows(ASOS_stations,temp_stations)
}

ASOS_stations <- ASOS_stations %>%
  rename(latitude=lat,
         longitude=lon)

#' Identify stations near county DEC monitors
lat_lon_df <- data.frame(id=c("02-020-0018","02-170-0008","02-020-1004","02-090-0034",
                              "02-020-1004/02-020-0018"),longitude=c(-149.82460,-149.031655,-149.56972,-147.72727,-149.6974),
                         latitude=c(61.205861,61.534163,61.32669,64.8458,61.26634))

ASOS_stations <- nearby_ASOS_stations(lat_lon_df,lat_colname="latitude",
                                    lon_colname="longitude",station_data=ASOS_stations,
                                    radius=160.934, limit=3)


#'Write function to clean `riem_measures` output 
getmode <- function(v) {
  uniqv <- unique(v)[which(!is.na(unique(v)))]
  uniqv[which.max(tabulate(match(v[which(!is.na(v))], uniqv)))]
}
pull_riem <- function(test_station_ID) {
  test_riem_measures <- riem_measures(station = test_station_ID, date_start = "2015-01-01",
                                      date_end = "2019-12-31") %>%
    mutate(date=as.Date(valid,tryFormats=c("%Y-%m-%d %H:%M:%S"))) %>%
    select(station,date,tmpf,relh,valid) %>%
    group_by(station,date)
  tmpf_non_NAs <- test_riem_measures %>%
    filter(!is.na(tmpf)) %>%
    group_by(station) %>%
    mutate(interval = as.character.POSIXt(valid-lag(valid, order_by = valid))) 
  relh_non_NAs <- test_riem_measures %>%
    filter(!is.na(relh)) %>%
    group_by(station) %>%
    mutate(interval = as.character.POSIXt(valid-lag(valid, order_by = valid))) 
  
  for (i in 1:nrow(tmpf_non_NAs)) {
    if (grepl("min",tmpf_non_NAs[i,"interval"])) {
      tmpf_non_NAs[i,"interval"] <- as.double(str_split(tmpf_non_NAs[i,"interval"]," ")[[1]][2])/60
    } else {
      tmpf_non_NAs[i,"interval"] <- as.double(str_split(tmpf_non_NAs[i,"interval"]," ")[[1]][2])
    }
  }
  for (i in 1:nrow(relh_non_NAs)) {
    if (grepl("min",relh_non_NAs[i,"interval"])) {
      relh_non_NAs[i,"interval"] <- as.double(str_split(relh_non_NAs[i,"interval"]," ")[[1]][2])/60
    } else {
      relh_non_NAs[i,"interval"] <- as.double(str_split(relh_non_NAs[i,"interval"]," ")[[1]][2])
    }
  }
  
  tmpf_non_NAs <- tmpf_non_NAs %>%
    mutate(tmpf_interval = getmode(interval)) %>%
    select(tmpf_interval,station) %>%
    distinct()
  relh_non_NAs <- relh_non_NAs %>%
    mutate(relh_interval = getmode(interval)) %>%
    select(relh_interval,station) %>%
    distinct()
  
  test_riem_measures_temp <- test_riem_measures %>%
    filter(!is.na(tmpf)) %>%
    full_join(tmpf_non_NAs,by=c("station")) %>%
    group_by(date) %>%
    add_tally() %>%
    group_by(station,date,tmpf_interval,n) %>%
    summarise(mean_tmpf=mean(tmpf,na.rm=TRUE),
              max_tmpf=max(tmpf,na.rm=TRUE)) %>%
    mutate(pct_reading_temp=(n/(24/as.double(tmpf_interval)))*100) %>%
    ungroup() %>%
    select(-n) %>%
    rename(id=station)
  test_riem_measures_relh <- test_riem_measures %>%
    filter(!is.na(relh)) %>%
    full_join(relh_non_NAs,by=c("station")) %>%
    group_by(date) %>%
    add_tally() %>%
    group_by(station,date,relh_interval,n) %>%
    summarise(mean_relh=mean(relh,na.rm=TRUE)) %>%
    mutate(pct_reading_relh=(n/(24/as.double(relh_interval)))*100) %>%
    ungroup() %>%
    select(-n) %>%
    rename(id=station)
  test_riem_measures <- full_join(test_riem_measures_temp,test_riem_measures_relh,by=c("date","id"))
}

#' Iterate through identified stations for each county
total_weather_list <- list()
for (p in 1:length(ASOS_stations)) {
  temp_weather_df <- data.frame()
  for (i in 1:length(ASOS_stations[[p]]$id)) {
    weather_data <- pull_riem(ASOS_stations[[p]]$id[i])
    temp_weather_df <- bind_rows(temp_weather_df,weather_data)
  }
  total_weather_list[[p]]<-ASOS_stations[[p]] %>%
    full_join(temp_weather_df,by=c("id"))
}

total_weather_df <- data.frame()
for (i in 1:length(total_weather_list)) {
  temp_weather <- total_weather_list[[i]] %>%
    rename(station_id=id) %>%
    mutate(participant_id=i)
  total_weather_df <- bind_rows(total_weather_df,temp_weather)
}

####Identify nearest monitored data####
#'For each site/day, choose the weather data measured at the nearest ASOS monitor.
closest_weather_df <- data.frame()
for (i in 1:length(total_weather_list)) {
  temp_weather <- total_weather_list[[i]] %>%
    rename(station_id=id) %>%
    mutate(participant_id=i) %>%
    filter(distance==min(unique(distance),na.rm=TRUE))
  closest_weather_df <- bind_rows(closest_weather_df,temp_weather)
}

#'Also create dataframes of weather data measured at second- and third-closest ASOS
#'monitors.
second_closest_weather_df <- data.frame()
for (i in 1:length(total_weather_list)) {
  temp_weather <- total_weather_list[[i]] %>%
    rename(station_id=id) %>%
    mutate(participant_id=i) %>%
    filter(distance>min(unique(distance),na.rm=TRUE),
           distance<max(unique(distance),na.rm=TRUE))
  second_closest_weather_df <- bind_rows(second_closest_weather_df,temp_weather)
}

third_closest_weather_df <- data.frame()
for (i in 1:length(total_weather_list)) {
  temp_weather <- total_weather_list[[i]] %>%
    rename(station_id=id) %>%
    mutate(participant_id=i) %>%
    filter(distance==max(unique(distance),na.rm=TRUE))
  third_closest_weather_df <- bind_rows(third_closest_weather_df,temp_weather)
}

#'Clean up dataframes
for(i in 1:nrow(closest_weather_df)) {
  if (closest_weather_df[i,"participant_id"]==1) {
    closest_weather_df[i,"monitor_site"] <-"02-020-0018"
  } else if (closest_weather_df[i,"participant_id"]==2) {
    closest_weather_df[i,"monitor_site"] <- "02-020-1004"
  } else if (closest_weather_df[i,"participant_id"]==3) {
    closest_weather_df[i,"monitor_site"] <- "02-020-1004/02-020-0018"
  } else if (closest_weather_df[i,"participant_id"]==4) {
    closest_weather_df[i,"monitor_site"] <- "02-090-0034"
  } else if (closest_weather_df[i,"participant_id"]==5) {
    closest_weather_df[i,"monitor_site"] <- "02-170-0008"
  }
}
closest_temp_df <- closest_weather_df %>%
  filter(!is.na(mean_tmpf)) %>%
  filter(mean_tmpf!=-Inf) %>%
  filter(pct_reading_temp>75) %>% #Remove days with <75% of hourly measurements
  select(-participant_id,-mean_relh,-relh_interval,-pct_reading_relh) %>%
  rename(Site_ID=monitor_site)
closest_relh_df <- closest_weather_df %>%
  filter(!is.na(mean_relh)) %>%
  filter(mean_relh!=-Inf) %>%
  filter(pct_reading_relh>75) %>% #Remove days with <75% of hourly measurements
  select(-participant_id,-mean_tmpf,-max_tmpf,-tmpf_interval,-pct_reading_temp) %>%
  rename(Site_ID=monitor_site)
closest_weather_df <- full_join(closest_relh_df,closest_temp_df,by=c("station_id","name","latitude",
                                                                     "longitude","distance","date","Site_ID"))

for(i in 1:nrow(second_closest_weather_df)) {
  if (second_closest_weather_df[i,"participant_id"]==1) {
    second_closest_weather_df[i,"monitor_site"] <-"02-020-0018"
  } else if (second_closest_weather_df[i,"participant_id"]==2) {
    second_closest_weather_df[i,"monitor_site"] <- "02-020-1004"
  } else if (second_closest_weather_df[i,"participant_id"]==3) {
    second_closest_weather_df[i,"monitor_site"] <- "02-020-1004/02-020-0018"
  } else if (second_closest_weather_df[i,"participant_id"]==4) {
    second_closest_weather_df[i,"monitor_site"] <- "02-090-0034"
  } else if (second_closest_weather_df[i,"participant_id"]==5) {
    second_closest_weather_df[i,"monitor_site"] <- "02-170-0008"
  }
}
second_closest_temp_df <- second_closest_weather_df %>%
  filter(!is.na(mean_tmpf)) %>%
  filter(mean_tmpf!=-Inf) %>%
  filter(pct_reading_temp>75) %>% #Remove days with <75% of hourly measurements
  select(-participant_id,-mean_relh,-relh_interval,-pct_reading_relh) %>%
  rename(Site_ID=monitor_site)
second_closest_relh_df <- second_closest_weather_df %>%
  filter(!is.na(mean_relh)) %>%
  filter(mean_relh!=-Inf) %>%
  filter(pct_reading_relh>75) %>% #Remove days with <75% of hourly measurements
  select(-participant_id,-mean_tmpf,-max_tmpf,-tmpf_interval,-pct_reading_temp) %>%
  rename(Site_ID=monitor_site)
second_closest_weather_df <- full_join(second_closest_relh_df,second_closest_temp_df,by=c("station_id","name","latitude",
                                                                     "longitude","distance","date","Site_ID"))

for(i in 1:nrow(third_closest_weather_df)) {
  if (third_closest_weather_df[i,"participant_id"]==1) {
    third_closest_weather_df[i,"monitor_site"] <-"02-020-0018"
  } else if (third_closest_weather_df[i,"participant_id"]==2) {
    third_closest_weather_df[i,"monitor_site"] <- "02-020-1004"
  } else if (third_closest_weather_df[i,"participant_id"]==3) {
    third_closest_weather_df[i,"monitor_site"] <- "02-020-1004/02-020-0018"
  } else if (third_closest_weather_df[i,"participant_id"]==4) {
    third_closest_weather_df[i,"monitor_site"] <- "02-090-0034"
  } else if (third_closest_weather_df[i,"participant_id"]==5) {
    third_closest_weather_df[i,"monitor_site"] <- "02-170-0008"
  }
}
third_closest_temp_df <- third_closest_weather_df %>%
  filter(!is.na(mean_tmpf)) %>%
  filter(mean_tmpf!=-Inf) %>%
  filter(pct_reading_temp>75) %>% #Remove days with <75% of hourly measurements
  select(-participant_id,-mean_relh,-relh_interval,-pct_reading_relh) %>%
  rename(Site_ID=monitor_site)
third_closest_relh_df <- third_closest_weather_df %>%
  filter(!is.na(mean_relh)) %>%
  filter(mean_relh!=-Inf) %>%
  filter(pct_reading_relh>75) %>% #Remove days with <75% of hourly measurements
  select(-participant_id,-mean_tmpf,-max_tmpf,-tmpf_interval,-pct_reading_temp) %>%
  rename(Site_ID=monitor_site)
third_closest_weather_df <- full_join(third_closest_relh_df,third_closest_temp_df,by=c("station_id","name","latitude",
                                                                     "longitude","distance","date","Site_ID"))

####Select single daily value for temperature####
#' Merge closest ASOS monitor weather data with all dates of study and look for 
#' missing values to replace with next-closest temp data.
every.day <- seq(as.Date("2015-01-01"), as.Date("2019-12-30"), by="1 day")
every.day <- rep(every.day,5)
every.day <- data.frame(date=every.day,`Site_ID`=rep(c("02-020-0018","02-020-1004","02-020-1004/02-020-0018",
                                                       "02-090-0034","02-170-0008"),each=length(every.day)/5))
weather_data <- full_join(every.day,closest_weather_df,by=c("date","Site_ID"))

#'Identify days with missing nearest-monitor temperature data
missing_temp_obs <- weather_data[which(weather_data$date>="2015-01-01" &
                                             is.na(weather_data$max_tmpf)),] %>%
  select(date,Site_ID) %>%
  mutate(status="Missing closest data")

#'Pull temperature data from second-closest ASOS monitor for those days
replacement_temp_data <- second_closest_temp_df %>%
  full_join(missing_temp_obs, by=c("date","Site_ID")) %>%
  filter(!is.na(status))

replacement_temp_data <- weather_data %>%
  full_join(missing_temp_obs,by=c("date","Site_ID")) %>%
  filter(!is.na(status)) %>%
  select(-latitude,-longitude,-mean_tmpf,-max_tmpf,-station_id,-name,-distance,
         -pct_reading_temp,-status,-tmpf_interval) %>%
  full_join(replacement_temp_data,by=c("date","Site_ID"))

weather_data <- weather_data %>%
  full_join(missing_temp_obs,by=c("date","Site_ID")) %>%
  filter(is.na(status)) %>%
  bind_rows(replacement_temp_data)

#'Identify days with missing nearest and second-nearest monitor temperature data
missing_temp_obs <- weather_data[which(weather_data$date>="2015-01-01" &
                                             is.na(weather_data$max_tmpf)),] %>%
  select(date,Site_ID) %>%
  mutate(status="Missing second-closest data")
replacement_temp_data <- third_closest_temp_df %>%
  full_join(missing_temp_obs, by=c("date","Site_ID")) %>%
  filter(!is.na(status))

#'Pull temperature data from third-closest ASOS monitor for those days
replacement_temp_data <- weather_data %>%
  select(-status) %>%
  full_join(missing_temp_obs,by=c("date","Site_ID")) %>%
  filter(!is.na(status)) %>%
  select(-latitude,-longitude,-mean_tmpf,-max_tmpf,-station_id,-name,-distance,
         -pct_reading_temp,-tmpf_interval,-status) %>%
  full_join(replacement_temp_data,by=c("date","Site_ID"))

weather_data <- weather_data %>%
  select(-status) %>%
  full_join(missing_temp_obs,by=c("date","Site_ID")) %>%
  filter(is.na(status)) %>%
  bind_rows(replacement_temp_data)

####Select single daily value for temperature####
#'Identify days with missing nearest-monitor relative humidity data
missing_relh_obs <- weather_data[which(weather_data$date>="2015-01-01" &
                                             is.na(weather_data$mean_relh)),] %>%
  select(date,Site_ID) %>%
  mutate(status="Missing closest data")

#'Pull relative humidity data from second-closest ASOS monitor for those days
replacement_relh_data <- second_closest_relh_df %>%
  full_join(missing_relh_obs, by=c("date","Site_ID")) %>%
  filter(!is.na(status))

replacement_relh_data <- weather_data %>%
  select(-status) %>%
  full_join(missing_relh_obs,by=c("date","Site_ID")) %>%
  filter(!is.na(status)) %>%
  select(-latitude,-longitude,-mean_relh,-mean_relh,-station_id,-name,-distance,
         -pct_reading_relh,-status,-relh_interval) %>%
  full_join(replacement_relh_data,by=c("date","Site_ID"))

weather_data <- weather_data %>%
  select(-status) %>%
  full_join(missing_relh_obs,by=c("date","Site_ID")) %>%
  filter(is.na(status)) %>%
  bind_rows(replacement_relh_data)

#'Identify days with missing nearest and second-nearest monitor relative humidity data
missing_relh_obs <- weather_data[which(weather_data$date>="2015-01-01" &
                                             is.na(weather_data$mean_relh)),] %>%
  select(date,Site_ID) %>%
  mutate(status="Missing second-closest data")

#'Pull relative humidity data from third-closest ASOS monitor for those days
replacement_relh_data <- third_closest_relh_df %>%
  full_join(missing_relh_obs, by=c("date","Site_ID")) %>%
  filter(!is.na(status))

replacement_relh_data <- weather_data %>%
  select(-status) %>%
  full_join(missing_relh_obs,by=c("date","Site_ID")) %>%
  filter(!is.na(status)) %>%
  select(-latitude,-longitude,-mean_relh,-mean_relh,-station_id,-name,-distance,
         -pct_reading_relh,-relh_interval,-status) %>%
  full_join(replacement_relh_data,by=c("date","Site_ID"))

weather_data <- weather_data %>%
  select(-status) %>%
  full_join(missing_relh_obs,by=c("date","Site_ID")) %>%
  filter(is.na(status)) %>%
  bind_rows(replacement_relh_data)

weather_data <- weather_data %>%
  select(-status)

#'Read in DEC PM2.5 data
DEC_data <- read.csv("../clean_data/averaged_DEC_data.csv") %>%
  mutate(date=as.Date(Date,tryFormats=c("%m/%d/%Y"))) %>%
  select(-Date)

#'Merge relative humidity data with PM2.5 data
weather_DEC_data <- DEC_data %>%
  full_join(weather_data,by=c("date","Site_ID"))

weather_DEC_data <- weather_DEC_data %>%
  mutate(`COUNTY.NAME`=ifelse(grepl("02-020",Site_ID),"Anchorage",
                              ifelse(grepl("02-170",Site_ID),"Matanuska-Susitna",
                                     "Fairbanks North Star")))

####Save final clean dataset####
write.csv(weather_DEC_data,"../clean_data/weather_DEC_data.csv")