## ---------------------------
## Script name: Finalize exposure dataset
## Author: Grace Kuiper
## Version: 08/11/2020

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
new_daily_df <- select(new_daily_df,-n,-temp_mean_PM)

####Impute for PM2.5 for Anchorage####
Anchorage_daily_df <- new_daily_df %>%
  filter(COUNTY.NAME=="Anchorage")
for (i in 1:nrow(Anchorage_daily_df)) {
  if (i==1) {
    Anchorage_daily_df[i,"group"]=0
  } else {
    if (is.na(Anchorage_daily_df[i,"ARITHMETIC.MEAN"])) {
      if (is.na(Anchorage_daily_df[i-1,"ARITHMETIC.MEAN"])) {
        Anchorage_daily_df[i,"group"]=Anchorage_daily_df[i-1,"group"]
      } else if (!is.na(Anchorage_daily_df[i-1,"ARITHMETIC.MEAN"])) {
        Anchorage_daily_df[i,"group"]=Anchorage_daily_df[i-1,"group"]+1
      }
    } else if (!is.na(Anchorage_daily_df[i,"ARITHMETIC.MEAN"])) {
      Anchorage_daily_df[i,"group"]=Anchorage_daily_df[i-1,"group"]+1
    }
  }
} #Create a variable called `group` that will assign consecutive days with missing
  #PM2.5 data to the same group, so those values can be imputed if the duration of
  #the group of missing data days is less than or equal to two.
Anchorage_daily_df <- Anchorage_daily_df %>%
  group_by(group) %>%
  add_tally() %>%
  ungroup() %>%
  mutate(group=ifelse(is.na(ARITHMETIC.MEAN),group,NA),
         n=ifelse(is.na(ARITHMETIC.MEAN),n,NA))#Count the number of days in a row
                                               #with missing PM2.5 data

#'Impute PM2.5 from leading and lagging days if the missingness is only for one or
#'two consecutive days
Anchorage_daily_df <- Anchorage_daily_df %>%
  mutate(ARITHMETIC.MEAN=ifelse(!is.na(n) & n==1,(lag(ARITHMETIC.MEAN)+lead(ARITHMETIC.MEAN))/2,ARITHMETIC.MEAN))
Anchorage_daily_df <- Anchorage_daily_df %>%
  mutate(ARITHMETIC.MEAN=ifelse(!is.na(n) & n==2 & !is.na(lag(ARITHMETIC.MEAN)),(lag(ARITHMETIC.MEAN)+lead(ARITHMETIC.MEAN,2))/2,
                                ifelse(!is.na(n) & n==2 & is.na(lag(ARITHMETIC.MEAN)),(lag(ARITHMETIC.MEAN,2)+lead(ARITHMETIC.MEAN))/2,
                                ARITHMETIC.MEAN))) #16 days missing during study period

####Impute for PM2.5 for Fairbanks####
Fairbanks_daily_df <- new_daily_df %>%
  filter(COUNTY.NAME=="Fairbanks North Star")
for (i in 1:nrow(Fairbanks_daily_df)) {
  if (i==1) {
    Fairbanks_daily_df[i,"group"]=0
  } else {
    if (is.na(Fairbanks_daily_df[i,"ARITHMETIC.MEAN"])) {
      if (is.na(Fairbanks_daily_df[i-1,"ARITHMETIC.MEAN"])) {
        Fairbanks_daily_df[i,"group"]=Fairbanks_daily_df[i-1,"group"]
      } else if (!is.na(Fairbanks_daily_df[i-1,"ARITHMETIC.MEAN"])) {
        Fairbanks_daily_df[i,"group"]=Fairbanks_daily_df[i-1,"group"]+1
      }
    } else if (!is.na(Fairbanks_daily_df[i,"ARITHMETIC.MEAN"])) {
      Fairbanks_daily_df[i,"group"]=Fairbanks_daily_df[i-1,"group"]+1
    }
  }
}
Fairbanks_daily_df <- Fairbanks_daily_df %>%
  group_by(group) %>%
  add_tally() %>%
  ungroup() %>%
  mutate(group=ifelse(is.na(ARITHMETIC.MEAN),group,NA),
         n=ifelse(is.na(ARITHMETIC.MEAN),n,NA))

Fairbanks_daily_df <- Fairbanks_daily_df %>%
  mutate(ARITHMETIC.MEAN=ifelse(!is.na(n) & n==1,(lag(ARITHMETIC.MEAN)+lead(ARITHMETIC.MEAN))/2,ARITHMETIC.MEAN))
Fairbanks_daily_df <- Fairbanks_daily_df %>%
  mutate(ARITHMETIC.MEAN=ifelse(!is.na(n) & n==2 & !is.na(lag(ARITHMETIC.MEAN)),(lag(ARITHMETIC.MEAN)+lead(ARITHMETIC.MEAN,2))/2,
                                ifelse(!is.na(n) & n==2 & is.na(lag(ARITHMETIC.MEAN)),(lag(ARITHMETIC.MEAN,2)+lead(ARITHMETIC.MEAN))/2,
                                       ARITHMETIC.MEAN))) #19 days missing during study period

####Impute for PM2.5 for Matanuska-Susitna####
Matsu_daily_df <- new_daily_df %>%
  filter(COUNTY.NAME=="Matanuska-Susitna")
for (i in 1:nrow(Matsu_daily_df)) {
  if (i==1) {
    Matsu_daily_df[i,"group"]=0
  } else {
    if (is.na(Matsu_daily_df[i,"ARITHMETIC.MEAN"])) {
      if (is.na(Matsu_daily_df[i-1,"ARITHMETIC.MEAN"])) {
        Matsu_daily_df[i,"group"]=Matsu_daily_df[i-1,"group"]
      } else if (!is.na(Matsu_daily_df[i-1,"ARITHMETIC.MEAN"])) {
        Matsu_daily_df[i,"group"]=Matsu_daily_df[i-1,"group"]+1
      }
    } else if (!is.na(Matsu_daily_df[i,"ARITHMETIC.MEAN"])) {
      Matsu_daily_df[i,"group"]=Matsu_daily_df[i-1,"group"]+1
    }
  }
}
Matsu_daily_df <- Matsu_daily_df %>%
  group_by(group) %>%
  add_tally() %>%
  ungroup() %>%
  mutate(group=ifelse(is.na(ARITHMETIC.MEAN),group,NA),
         n=ifelse(is.na(ARITHMETIC.MEAN),n,NA))

Matsu_daily_df <- Matsu_daily_df %>%
  mutate(ARITHMETIC.MEAN=ifelse(!is.na(n) & n==1,(lag(ARITHMETIC.MEAN)+lead(ARITHMETIC.MEAN))/2,ARITHMETIC.MEAN))
Matsu_daily_df <- Matsu_daily_df %>%
  mutate(ARITHMETIC.MEAN=ifelse(!is.na(n) & n==2 & !is.na(lag(ARITHMETIC.MEAN)),(lag(ARITHMETIC.MEAN)+lead(ARITHMETIC.MEAN,2))/2,
                                ifelse(!is.na(n) & n==2 & is.na(lag(ARITHMETIC.MEAN)),(lag(ARITHMETIC.MEAN,2)+lead(ARITHMETIC.MEAN))/2,
                                       ARITHMETIC.MEAN))) #48 missing during study period

####Merge and save final exposure dataset####
daily_df_final <- bind_rows(Anchorage_daily_df,Fairbanks_daily_df,Matsu_daily_df) %>%
  select(-n,-group)

daily_df_final <- daily_df_final %>%
  mutate(Site_ID=as.character(Site_ID)) %>%
  mutate(Site_ID=ifelse(!is.na(Site_ID),Site_ID,
                        ifelse(COUNTY.NAME=="Anchorage","02-020-1004/02-020-0018",
                               ifelse(COUNTY.NAME=="Fairbanks North Star","02-090-0034",
                                      "02-170-0008"))))
daily_df_final <- select(daily_df_final,-X.2,-X.1)

write.csv(daily_df_final,"../clean_data/final_exp_data.csv")


