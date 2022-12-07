## ---------------------------
## Script name: Finalize exposure dataset
## Author: Grace Kuiper
## Version: 12/07/2022

## Purpose: Finishing touches on dataset cleaning for DEC data and
##          weather covariate data: impute missing days, remove
##          duplicate data, etc.

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

####Read in and clean exposure dataset####
exp_df <- read.csv("../raw_data/weather_DEC_data.csv") %>%
  select(-X) %>%
  mutate(date=as.Date(date))

#'Create dataframe of every day from 2009-2019
every.day <- seq(min(as.Date(exp_df$date)), max(as.Date(exp_df$date)), by="1 day")
every.day <- rep(every.day,3)
every.day <- data.frame(date=every.day,`COUNTY.NAME`=rep(c("Anchorage","Fairbanks North Star",
                                                           "Matanuska-Susitna"),each=length(every.day)/3))
daily_df <- exp_df %>%
  arrange(COUNTY.NAME,date) %>%
  full_join(every.day, by=c("COUNTY.NAME","date"))

#'Identify duplicated days
duplicate_data <- daily_df %>%
  group_by(`COUNTY.NAME`,`date`) %>%
  add_tally() %>%
  filter(n>1) %>%
  arrange(date,`COUNTY.NAME`) #All are due to multiple sites in Anchorage

#'Remove duplicate observations for multiple Anchorage sites
daily_df <- daily_df %>%
  group_by(`COUNTY.NAME`,`date`) %>%
  add_tally() %>%
  mutate(temp_mean_PM=mean(ARITHMETIC.MEAN,na.rm=TRUE))
new_daily_df <- as.data.frame(daily_df[1,])
for (i in 2:nrow(daily_df)) {
  if (i/200==ceiling(i/200)) {
    print(paste0("Working on ",i," of ",nrow(daily_df)))
  }
  if (daily_df[i,"n"]==1) {
    new_daily_df[nrow(new_daily_df)+1,] <- daily_df[i,]
  } else if(daily_df[i,"n"]>1) {
    if(is.na(daily_df[i,"temp_mean_PM"])) {
      if (daily_df[i,"Site_ID"]=="02-020-1004/02-020-0018") {
        new_daily_df[nrow(new_daily_df)+1,] <- daily_df[i,]
      }
    } else if (!is.na(daily_df[i,"temp_mean_PM"]) & !is.na(daily_df[i,"ARITHMETIC.MEAN"]) &
               daily_df[i,"ARITHMETIC.MEAN"]==daily_df[i,"temp_mean_PM"]) {
      new_daily_df[nrow(new_daily_df)+1,] <- daily_df[i,]
    }
  }
}
new_daily_df <- dplyr::select(new_daily_df,-n,-temp_mean_PM)

####Impute for PM2.5 for Anchorage####
new_daily_df_mod <- new_daily_df %>%
  arrange(COUNTY.NAME,date) %>%
  group_by(COUNTY.NAME) %>%
  mutate(ARITHMETIC.MEAN_lag1=lag(ARITHMETIC.MEAN,1),
         ARITHMETIC.MEAN_lag2=lag(ARITHMETIC.MEAN,2),
         ARITHMETIC.MEAN_lead1=lead(ARITHMETIC.MEAN,1),
         ARITHMETIC.MEAN_lead2=lead(ARITHMETIC.MEAN,2)) %>%
  mutate(ARITHMETIC.MEAN=ifelse(is.na(ARITHMETIC.MEAN),
                                (ARITHMETIC.MEAN_lag1+ARITHMETIC.MEAN_lead1)/2,
                                ARITHMETIC.MEAN)) %>%
  mutate(ARITHMETIC.MEAN=ifelse(is.na(ARITHMETIC.MEAN) & is.na(ARITHMETIC.MEAN_lag1),
                                (ARITHMETIC.MEAN_lag2+ARITHMETIC.MEAN_lead1)/2,
                                ifelse(is.na(ARITHMETIC.MEAN) & is.na(ARITHMETIC.MEAN_lead1),
                                       (ARITHMETIC.MEAN_lag1+ARITHMETIC.MEAN_lead2)/2,
                                       ARITHMETIC.MEAN)))

####Merge and save final exposure dataset####

daily_df_final <- new_daily_df_mod

daily_df_final <- daily_df_final %>%
  mutate(Site_ID=as.character(Site_ID)) %>%
  mutate(Site_ID=ifelse(!is.na(Site_ID),Site_ID,
                        ifelse(COUNTY.NAME=="Anchorage","02-020-1004/02-020-0018",
                               ifelse(COUNTY.NAME=="Fairbanks North Star","02-090-0034",
                                      "02-170-0008"))))
daily_df_final <- daily_df_final %>%
  dplyr::select(-contains("lead"),-contains("lag"),
                -contains("less"))

write.csv(daily_df_final,"../clean_data/final_exp_data.csv")