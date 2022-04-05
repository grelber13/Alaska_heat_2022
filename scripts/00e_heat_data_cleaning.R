## ---------------------------
## Script name: Heat index exposure data cleaning for AK heat analysis
## Author: Grace Kuiper
## Version: 01/11/2022

## Purpose: For the purpose of AK heat analysis using heat index 
## based on maximum daily temperature: clean heat index data

## Email: grace.kuiper@colostate.edu
## ---------------------------

library(tidyverse)
library(dplyr)

#'Read in finalized exposure dataset
final_exp_data <- read.csv('../raw_data/final_exp_data.csv') %>%
  select(-X)

#'Calculate daily HI using a multiple regression equation with adjustments 
#'specified by the National Weather Service within relevant ranges of 
#'temperature and humidity (Steadman 1979a, 1979b).
for (i in 1:nrow(final_exp_data)) {
  temp<-final_exp_data[i,"max_tmpf"]
  relh<-final_exp_data[i,"mean_relh"]
  if(!is.na(temp) & !is.na(relh)) {
    HI_test <- mean(0.5 * (temp+61 + ((temp-68)*1.2) + (relh*0.094)),temp)
    if (HI_test>80) {
      HI=-42.379 + 2.04901523*temp + 10.14333127*relh - .22475541*temp*relh - .00683783*temp*temp - .05481717*relh*relh + .00122874*temp*temp*relh + .00085282*temp*relh*relh - .00000199*temp*temp*relh*relh
      if (relh<13 & temp>80 & temp<112) {
        HI=HI-(((13-relh)/4)*(((17-abs(temp-95.))/17)^(1/2))) }
      if (relh>85 & temp>80 & temp<87) {
        HI=HI+(((relh-85)/10)*((87-temp)/5))
      }
    } else {
      HI=0.5 * (temp+61 + ((temp-68)*1.2) + (relh*0.094))
    }
    final_exp_data[i,"HI"] <- HI
  } else {final_exp_data[i,"HI"] <- NA}
}

#' Generate dataframes with lagged exposure data
HI_lag0_df <- final_exp_data %>%
  select(COUNTY.NAME,date,HI) %>%
  mutate(Date=as.Date(date)) %>%
  select(-date)
HI_lag1_df <- final_exp_data %>%
  select(COUNTY.NAME,date,HI) %>%
  mutate(Date=as.Date(date)+1) %>%
  select(-date) %>%
  rename(HI_lag1=HI)
HI_lag2_df <- final_exp_data %>%
  select(COUNTY.NAME,date,HI) %>%
  mutate(Date=as.Date(date)+2) %>%
  select(-date) %>%
  rename(HI_lag2=HI)
HI_lag3_df <- final_exp_data %>%
  select(COUNTY.NAME,date,HI) %>%
  mutate(Date=as.Date(date)+3) %>%
  select(-date) %>%
  rename(HI_lag3=HI)
HI_lag4_df <- final_exp_data %>%
  select(COUNTY.NAME,date,HI) %>%
  mutate(Date=as.Date(date)+4) %>%
  select(-date) %>%
  rename(HI_lag4=HI)
HI_lag5_df <- final_exp_data %>%
  select(COUNTY.NAME,date,HI) %>%
  mutate(Date=as.Date(date)+5) %>%
  select(-date) %>%
  rename(HI_lag5=HI)
HI_exp <- full_join(HI_lag0_df,
                    HI_lag1_df,by=c("COUNTY.NAME","Date")) %>%
  full_join(HI_lag2_df,by=c("COUNTY.NAME","Date")) %>%
  full_join(HI_lag3_df,by=c("COUNTY.NAME","Date")) %>%
  full_join(HI_lag4_df,by=c("COUNTY.NAME","Date")) %>%
  full_join(HI_lag5_df,by=c("COUNTY.NAME","Date"))

#' Read in case-crossover dataset from `00d_HFDR_data_cleaning.R` 
casecross_list <- readRDS("../clean_data/casecross_list.rds") %>%
  map(~filter(.,grepl("-06-|-07-|-08-",Date)))
casecross_list <- casecross_list %>%
  map(~left_join(.,HI_exp %>%
                   rename(county=COUNTY.NAME) %>%
                   mutate(county=as.character(county)),
                 by=c("Date","county")))

casecross_list <- casecross_list %>%
  map(~group_by(.,hfdrepisode)) %>%
  map(~filter(.,length(unique(outcome))==2))#Only include individuals for whom there
                                            #are both case and control days in the
                                            #case-crossover dataset.

saveRDS(casecross_list,"../clean_data/casecross_list_HI.rds")
