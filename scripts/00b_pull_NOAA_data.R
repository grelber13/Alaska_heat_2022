## ---------------------------
## Script name: Pull NOAA weather data
## Author: Grace Kuiper
## Version: 12/07/2022

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
hours <- seq(as.POSIXct("2015-01-01 00:00:00"), as.POSIXct("2019-12-31 23:00:00"), by="hour")
hours_df <- data.frame(valid=hours) %>%
  mutate(date=as.Date(substr(valid,1,10),tryFormats=c("%Y-%m-%d")),
         hour_char=substr(valid,1,14))

pull_riem <- function(test_station_ID) {
  hours_df_temp <- hours_df %>%
    dplyr::select(-valid)
  hours_df_temp$station=test_station_ID
  riem_df <- riem_measures(station = test_station_ID, date_start = "2015-01-01",
                           date_end = "2020-01-01") %>%
    mutate(date=as.Date(valid,tryFormats=c("%Y-%m-%d %H:%M:%S"))) %>%
    dplyr::select(station,date,tmpf,relh,valid) %>%
    mutate(hour_char=substr(valid,1,14))
  
  if (nrow(riem_df)>0) {
    riem_df <- right_join(riem_df,hours_df_temp,
                          by=c("station","date","hour_char")) %>%
      arrange(date,hour_char)
    
    riem_hr_df <- riem_df %>%
      mutate(HI_test = ifelse(!is.na(tmpf)&!is.na(relh),
                              ((0.5 * (tmpf+61 + ((tmpf-68)*1.2) + (relh*0.094)))+tmpf)/2,NA)) %>%
      mutate(hr_HI_temp=ifelse(HI_test>80,
                               -42.379 + 2.04901523*tmpf + 10.14333127*relh - .22475541*tmpf*relh - .00683783*tmpf*tmpf - .05481717*relh*relh + .00122874*tmpf*tmpf*relh + .00085282*tmpf*relh*relh - .00000199*tmpf*tmpf*relh*relh,
                               0.5*(tmpf+61+((tmpf-68)*1.2)+(relh*0.094)))) %>%
      mutate(hr_HI=ifelse(HI_test<=80,
                          hr_HI_temp,
                          ifelse(relh<13 & tmpf<112,
                                 hr_HI_temp-(((13-relh)/4)*(((17-abs(tmpf-95))/17)^(1/2))),
                                 ifelse(relh>85 & tmpf<87,
                                        hr_HI_temp+(((relh-85)/10)*((87-tmpf)/5)),
                                        hr_HI_temp)))) %>%
      group_by(station,hour_char,date) %>%
      filter(hr_HI==max(hr_HI,na.rm=T)) %>%
      rename(hr_tmpf=tmpf,
             hr_relh=relh) %>%
      dplyr::select(-HI_test,-hr_HI_temp)
    
    riem_day_df <- riem_hr_df %>%
      mutate(hr_meas_tmpf=ifelse(is.na(hr_tmpf),0,1),
             hr_meas_relh=ifelse(is.na(hr_relh),0,1),
             hr_meas_HI=ifelse(is.na(hr_HI),0,1)) %>%
      group_by(station,date) %>%
      summarise(day_tmpf=max(hr_tmpf,na.rm=T),
                day_relh=mean(hr_relh,na.rm=T),
                day_HI=max(hr_HI,na.rm=T),
                n_hr_meas_tmpf=sum(hr_meas_tmpf),
                n_hr_meas_relh=sum(hr_meas_relh),
                n_hr_meas_HI=sum(hr_meas_HI),
                .groups='drop')
    
    riem_day_df <- riem_day_df %>%
      mutate(less_18hr_meas_tmpf=ifelse(n_hr_meas_tmpf<18,1,0),
             less_18hr_meas_relh=ifelse(n_hr_meas_relh<18,1,0),
             less_18hr_meas_HI=ifelse(n_hr_meas_relh<18,1,0),
             less_12hr_meas_tmpf=ifelse(n_hr_meas_tmpf<12,1,0),
             less_12hr_meas_relh=ifelse(n_hr_meas_relh<12,1,0),
             less_12hr_meas_HI=ifelse(n_hr_meas_relh<12,1,0)) %>%
      rename(id=station)
  } else {riem_day_df <- riem_df}
  return(riem_day_df)
}

#' Iterate through identified stations for each county
total_weather_list <- list()
for (p in 1:length(ASOS_stations)) {
  temp_weather_df <- data.frame()
  for (i in 1:length(ASOS_stations[[p]]$id)) {
    print(paste0("Working on ",i, " of ",length(ASOS_stations[[p]]$id),
                 " for ",p," of ", length(ASOS_stations)))
    weather_data <- pull_riem(ASOS_stations[[p]]$id[i])
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
closest_HI_df <- closest_weather_df %>%
  filter(!is.na(day_HI)) %>%
  filter(day_tmpf!=-Inf) %>%
  filter(less_18hr_meas_HI==0) %>%
  dplyr::select(-participant_id) %>%
  rename(Site_ID=monitor_site)


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
second_closest_HI_df <- second_closest_weather_df %>%
  filter(!is.na(day_HI)) %>%
  filter(day_tmpf!=-Inf) %>%
  filter(less_18hr_meas_HI==0) %>%
  dplyr::select(-participant_id) %>%
  rename(Site_ID=monitor_site)


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
third_closest_HI_df <- third_closest_weather_df %>%
  filter(!is.na(day_HI)) %>%
  filter(day_tmpf!=-Inf) %>%
  filter(less_18hr_meas_HI==0) %>%
  dplyr::select(-participant_id) %>%
  rename(Site_ID=monitor_site)

####Select single daily value for temperature####
#' Merge closest ASOS monitor weather data with all dates of study and look for 
#' missing values to replace with next-closest temp data.
every.day <- seq(as.Date("2015-01-01"), as.Date("2019-12-30"), by="1 day")
every.day <- rep(every.day,5)
every.day <- data.frame(date=every.day,`Site_ID`=rep(c("02-020-0018","02-020-1004","02-020-1004/02-020-0018",
                                                       "02-090-0034","02-170-0008"),each=length(every.day)/5))
weather_data <- full_join(every.day,closest_HI_df,by=c("date","Site_ID"))

#'Identify days with missing nearest monitor HI data
missing_HI_obs <- weather_data[which(weather_data$date>="2015-01-01" &
                                       is.na(weather_data$day_HI)),] %>%
  dplyr::select(date,Site_ID) %>%
  mutate(status="Missing closest data")

#'Pull HI data from second-closest ASOS monitor for those days
replacement_HI_data <- second_closest_HI_df %>%
  full_join(missing_HI_obs, by=c("date","Site_ID")) %>%
  filter(!is.na(status))

weather_data <- weather_data %>%
  full_join(missing_HI_obs,by=c("date","Site_ID")) %>%
  filter(is.na(status)) %>%
  bind_rows(replacement_HI_data) %>%
  dplyr::select(-status)

#'Identify days with missing nearest and second-nearest monitor HI data
missing_HI_obs <- weather_data[which(weather_data$date>="2015-01-01" &
                                       is.na(weather_data$day_HI)),] %>%
  dplyr::select(date,Site_ID) %>%
  mutate(status="Missing second-closest data")

#'Pull HI data from third-closest ASOS monitor for those days
replacement_HI_data <- third_closest_HI_df %>%
  full_join(missing_HI_obs, by=c("date","Site_ID")) %>%
  filter(!is.na(status))

weather_data <- weather_data %>%
  full_join(missing_HI_obs,by=c("date","Site_ID")) %>%
  filter(is.na(status)) %>%
  bind_rows(replacement_HI_data)

#' Impute HI for missing single or double days											  
weather_data <- weather_data %>%
  select(-status) %>%
  arrange(Site_ID,date) %>%
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
  mutate(month=substr(date,6,7))

for (i in 1:nrow(weather_DEC_data)) {
  if (grepl("01|02|03",weather_DEC_data[i,"month"])) {
    weather_DEC_data[i,"quarter"]<-"Quarter 1"
  } else if (grepl("04|05|06",weather_DEC_data[i,"month"])) {
    weather_DEC_data[i,"quarter"]<-"Quarter 2"
  } else if (grepl("07|08|09",weather_DEC_data[i,"month"])) {
    weather_DEC_data[i,"quarter"]<-"Quarter 3"
  } else if (grepl("10|11|12",weather_DEC_data[i,"month"])) {
    weather_DEC_data[i,"quarter"]<-"Quarter 4"
  }
}

weather_DEC_data <- weather_DEC_data %>%
  mutate(`COUNTY.NAME`=ifelse(grepl("02-020",Site_ID),"Anchorage",
                              ifelse(grepl("02-170",Site_ID),"Matanuska-Susitna",
                                     "Fairbanks North Star")))

####Save final clean dataset####
write.csv(weather_DEC_data,"../clean_data/weather_DEC_data.csv")