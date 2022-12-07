## ---------------------------
## Script name: Heat index exposure data cleaning for AK heat analysis
## Author: Grace Kuiper
## Version: 12/07/2022

## Purpose: For the purpose of AK heat analysis using heat index 
## based on maximum daily temperature: clean heat index data

## Email: grace.kuiper@colostate.edu
## ---------------------------

library(tidyverse)
library(dplyr)

#'Read in finalized exposure dataset
final_exp_data <- read.csv('../raw_data/final_exp_data.csv') %>%
  dplyr::select(-X) %>%
  rename(HI=day_HI)

#' Generate dataframes with lagged exposure data
HI_lag0_df <- final_exp_data %>%
  dplyr::select(COUNTY.NAME,date,HI) %>%
  mutate(Date=as.Date(date)) %>%
  dplyr::select(-date)
HI_lag1_df <- final_exp_data %>%
  dplyr::select(COUNTY.NAME,date,HI) %>%
  mutate(Date=as.Date(date)+1) %>%
  dplyr::select(-date) %>%
  rename(HI_lag1=HI)
HI_lag2_df <- final_exp_data %>%
  dplyr::select(COUNTY.NAME,date,HI) %>%
  mutate(Date=as.Date(date)+2) %>%
  dplyr::select(-date) %>%
  rename(HI_lag2=HI)
HI_lag3_df <- final_exp_data %>%
  dplyr::select(COUNTY.NAME,date,HI) %>%
  mutate(Date=as.Date(date)+3) %>%
  dplyr::select(-date) %>%
  rename(HI_lag3=HI)
HI_lag4_df <- final_exp_data %>%
  dplyr::select(COUNTY.NAME,date,HI) %>%
  mutate(Date=as.Date(date)+4) %>%
  dplyr::select(-date) %>%
  rename(HI_lag4=HI)
HI_lag5_df <- final_exp_data %>%
  dplyr::select(COUNTY.NAME,date,HI) %>%
  mutate(Date=as.Date(date)+5) %>%
  dplyr::select(-date) %>%
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
