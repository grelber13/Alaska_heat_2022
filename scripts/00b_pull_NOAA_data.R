## ---------------------------
## Script name: Pull NOAA weather data
## Author: Grace Kuiper
## Version: 03/29/2023

## Purpose: Pull NOAA temperature and RH data using `rnoaa` package. 
##          Calculated hourly head index and clean data to 
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

source("helper_functions/riem_helper_functions.R")

####Pull weather data from ASOS stations####
#'Locate nearby stations
AK_ASOS_networks <- c("AK_ASOS")
ASOS_stations_df <- data.frame()
for (i in 1:length(AK_ASOS_networks)) {
  temp_stations <- riem_stations(AK_ASOS_networks[i]) %>%
    mutate(network=AK_ASOS_networks[i])
  ASOS_stations_df <- bind_rows(ASOS_stations_df,temp_stations)
}

# ASOS_stations <- ASOS_stations %>%
#   rename(latitude=lat,
#          longitude=lon)

#' Identify stations near county DEC monitors
lat_lon_df <- data.frame(id=c("02-020-0018","02-170-0008","02-020-1004","02-090-0034",
                              "02-020-1004/02-020-0018"),longitude=c(-149.82460,-149.031655,-149.56972,-147.72727,-149.6974),
                         latitude=c(61.205861,61.534163,61.32669,64.8458,61.26634))

ASOS_stations <- nearby_ASOS_stations(lat_lon_df,lat_colname="latitude",
                                      lon_colname="longitude",station_data=ASOS_stations_df,
                                      radius=160.934, limit=4)

#' Iterate through identified stations for each county
total_weather_list <- list()
for (p in 1:length(ASOS_stations)) {
  temp_weather_df <- data.frame()
  for (i in 1:length(ASOS_stations[[p]]$id)) {
    print(paste0("Working on ",i, " of ",
                 length(ASOS_stations[[p]]$id),
                 " for ",p," of ", 
                 length(ASOS_stations)))
    weather_data <- pull_riem(test_station_ID=ASOS_stations[[p]]$id[i],
                              start_date="2000-01-01",
                              end_date="2020-12-31")
    if(nrow(weather_data)>0) {temp_weather_df <- bind_rows(temp_weather_df,weather_data)}
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
    group_by(date) %>%
    filter(distance==min(distance))
  closest_weather_df <- bind_rows(closest_weather_df,temp_weather)
}

#'Also create dataframes of weather data measured at second- and third-closest ASOS
#'monitors.
second_closest_weather_df <- data.frame()
for (i in 1:length(total_weather_list)) {
  temp_weather <- total_weather_list[[i]] %>%
    group_by(date) %>%
    filter(distance>min(distance)) %>%
    filter(distance==min(distance))
  second_closest_weather_df <- bind_rows(second_closest_weather_df,temp_weather)
}

third_closest_weather_df <- data.frame()
for (i in 1:length(total_weather_list)) {
  temp_weather <- total_weather_list[[i]] %>%
    group_by(date) %>%
    filter(distance>min(distance)) %>%
    filter(distance>min(distance)) %>%
    filter(distance==min(distance))
  third_closest_weather_df <- bind_rows(third_closest_weather_df,temp_weather)
}

#'Clean up dataframes
site_key_df <- data.frame(participant_id=c(1,2,3,4,5),
                          monitor_site=c("02-020-0018",
                                         "02-020-1004",
                                         "02-020-1004/02-020-0018",
                                         "02-090-0034",
                                         "02-170-0008"))
closest_HI_df <- closest_weather_df %>% 
  left_join(site_key_df,
            by=c("participant_id")) %>%
  filter(!is.na(day_HI)) %>%
  filter(day_tmpf!=-Inf) %>%
  filter(less_18hr_meas_HI==0) %>%
  dplyr::select(-participant_id) %>%
  rename(Site_ID=monitor_site)

second_closest_HI_df <- second_closest_weather_df %>%
  left_join(site_key_df,
            by=c("participant_id")) %>%
  filter(!is.na(day_HI)) %>%
  filter(day_tmpf!=-Inf) %>%
  filter(less_18hr_meas_HI==0) %>%
  dplyr::select(-participant_id) %>%
  rename(Site_ID=monitor_site)

third_closest_HI_df <- third_closest_weather_df %>%
  left_join(site_key_df,
            by=c("participant_id")) %>%
  filter(!is.na(day_HI)) %>%
  filter(day_tmpf!=-Inf) %>%
  filter(less_18hr_meas_HI==0) %>%
  dplyr::select(-participant_id) %>%
  rename(Site_ID=monitor_site)

####Select single daily value for temperature####
#' Merge closest ASOS monitor weather data with all dates of study and look for 
#' missing values to replace with next-closest temp data.
every.day <- seq(as.Date("2000-01-01"), as.Date("2020-12-31"), by="1 day")
every.day <- rep(every.day,5)
every.day <- data.frame(date=every.day,`Site_ID`=rep(c("02-020-0018","02-020-1004","02-020-1004/02-020-0018",
                                                       "02-090-0034","02-170-0008"),each=length(every.day)/5))
weather_data <- full_join(every.day,closest_HI_df,by=c("date","Site_ID"))

#'Identify days with missing nearest monitor HI data
missing_HI_obs <- weather_data %>%
  filter(is.na(day_HI)) %>%
  dplyr::select(date,Site_ID)

#'Pull HI data from second-closest ASOS monitor for those days
replacement_HI_data <- left_join(missing_HI_obs %>%
                                   mutate(missing=1),
                                 second_closest_HI_df,
                                 by=c("date","Site_ID")) %>%
  filter(missing==1) %>%
  dplyr::select(-missing)

weather_data <- weather_data  %>%
  filter(!is.na(day_HI)) %>%
  bind_rows(replacement_HI_data) %>%
  arrange(Site_ID,date)

#'Identify days with missing nearest and second-nearest monitor HI data
missing_HI_obs <- weather_data %>%
  filter(is.na(day_HI)) %>%
  dplyr::select(date,Site_ID)

#'Pull HI data from third-closest ASOS monitor for those days
replacement_HI_data <- left_join(missing_HI_obs %>%
                                   mutate(missing=1),
                                 third_closest_HI_df,
                                 by=c("date","Site_ID")) %>%
  filter(missing==1) %>%
  dplyr::select(-missing)

weather_data <- weather_data  %>%
  filter(!is.na(day_HI)) %>%
  bind_rows(replacement_HI_data) %>%
  arrange(Site_ID,date)

#' Impute HI for missing single or double days											  
weather_data <- weather_data %>%
  group_by(Site_ID) %>%
  mutate(day_HI_lag1=lag(day_HI,1),
         day_HI_lag2=lag(day_HI,2),
         day_HI_lead1=lead(day_HI,1),
         day_HI_lead2=lead(day_HI,2)) %>%
  mutate(day_HI=ifelse(is.na(day_HI),
                       (day_HI_lag1+day_HI_lead1)/2,
                       day_HI)) %>%
  mutate(day_HI=ifelse(is.na(day_HI) & is.na(day_HI_lag1),
                       (day_HI_lag2+day_HI_lead1)/2,
                       ifelse(is.na(day_HI) & is.na(day_HI_lead1),
                              (day_HI_lag1+day_HI_lead2)/2,
                              day_HI)))

#'Read in DEC PM2.5 data
DEC_data <- read.csv("../clean_data/averaged_DEC_data.csv") %>%
  mutate(date=as.Date(Date,tryFormats=c("%m/%d/%Y"))) %>%
  select(-Date)

#'Merge HI data with PM2.5 data
weather_DEC_data <- DEC_data %>%
  full_join(weather_data,by=c("date","Site_ID")) %>%
  mutate(month=substr(date,6,7)) %>%
  mutate(quarter=ifelse(grepl("01|02|03",month),
                        "Quarter 1",
                        ifelse(grepl("04|05|06",month),
                               "Quarter 2",
                               ifelse(grepl("07|08|09",month),
                                            "Quarter 3",
                                            "Quarter 4"))))

weather_DEC_data <- weather_DEC_data %>%
  mutate(`COUNTY.NAME`=ifelse(grepl("02-020",Site_ID),"Anchorage",
                              ifelse(grepl("02-170",Site_ID),"Matanuska-Susitna",
                                     "Fairbanks North Star")))

####Save final clean dataset####
write.csv(weather_DEC_data,"../clean_data/weather_DEC_data.csv")