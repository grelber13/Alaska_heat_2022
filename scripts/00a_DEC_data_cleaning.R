## ---------------------------
## Script name: DEC data cleaning
## Author: Grace Kuiper
## Version: 03/29/2023

## Purpose: For the purpose of AK heat analysis: clean DEC data for
##          PM2.5 covariate data

## Email: grace.kuiper@colostate.edu
## ---------------------------

library(tidyverse)
library(data.table)
library(lubridate)
library(eeptools)
library(dplyr)
library(readxl)

####Import Department of Environmental Conservation data####
DEC_data <- read_xlsx('../raw_data/daily_88101_0819_NCORE_Garden_Parkgate_Butte.xlsx', 
                      col_names = T)

#'Create Site ID variable
Site_ID_key_df <- data.frame(`COUNTY CODE`=c(20,20,170,90),
                             Site_No=c(18,1004,8,34),
                             Site_ID=c("02-020-0018",
                                       "02-020-1004",
                                       "02-170-0008",
                                       "02-090-0034")) %>%
  rename(`COUNTY CODE`=COUNTY.CODE)
DEC_data <- dplyr::rename(DEC_data,Site_No=`SITE ID`) %>%
  left_join(Site_ID_key_df,
            by=c("COUNTY CODE","Site_No"))

#'Format date variable
DEC_data <- DEC_data %>%
  mutate(Date=as.Date(as.character(`COLLECTION DATE`),tryFormats = c("%Y%m%d"))) %>%
  select(-`COLLECTION DATE`,-`YEAR`,-`MONTH`,-`DAY`)

#'Eliminate readings with fewer than 75% of measurements (18 hours) for summarized hourly data
DEC_data <- DEC_data %>%
  filter(`PCT DAILY OBS`>=75) %>%
  filter(ifelse(`DURATION DESC`=="1 HOUR",`NUM DAILY OBS`>=18,`NUM DAILY OBS`>0)) %>%
  filter(`ARITHMETIC MEAN`>0)

####Reduce dataset to only include one reading per day per study site####
#'This was a labor-intensive, manual approach. Some monitors were indicated by DEC
#'to have higher priority over others. Furthermore, it was preferable to have
#'averaged hourly measurements than single 24-hour measurements. On some days,
#'there was only one measurement fro a site; however, on others, there were multiple
#'measurements taken. So, first a dataset was created with all days during which
#'there was only one measurement at a single site. Then for sites that had tow or
#'more measurements on a given day, the one collected by the highest-priority monitor 
#'was selected, then if there were multiple high-priority measurmeents, hourly
#'measurements were preferentially selected. This process was repeated manually until
#'a single measurement was extracted for each site for each day on which measurements 
#'were taken.

#'First, assign priorities for different monitors using information provided by DEC.
POC_priority_key_df <- data.frame(Site_ID=c("02-020-0018",
                                            "02-020-1004",
                                            "02-170-0008",
                                            "02-170-0008",
                                            "02-090-0034",
                                            "02-090-0034",
                                            "02-090-0034"),
                                  POC=c(3,3,3,1,1,2,3),
                                  POC_priority=c(1,1,1,2,1,2,3))

DEC_data <- left_join(DEC_data,
                      POC_priority_key_df,
                      by=c("Site_ID","POC")) %>% 
  replace_na(list(POC_priority = 0))

#' Assign priorities based on criteria described above
meas_priority_key_df <- data.frame(POC_priority=c(1,1,1,
                                                  2,2,2,
                                                  3,3,3,
                                                  0,0,0),
                                   `DURATION DESC`=c("1 HOUR","24 HOUR","24-HR BLK AVG",
                                                     "1 HOUR","24 HOUR","24-HR BLK AVG",
                                                     "1 HOUR","24 HOUR","24-HR BLK AVG",
                                                     "1 HOUR","24 HOUR","24-HR BLK AVG"),
                                   meas_priority=c(1,2,2,
                                                   3,4,4,
                                                   5,6,6,
                                                   7,8,8)) %>%
  rename(`DURATION DESC`=DURATION.DESC)


DEC_data_merge <- DEC_data %>%
  left_join(meas_priority_key_df,
            by=c("POC_priority",
                 "DURATION DESC"))

DEC_priority <- DEC_data_merge %>%
  group_by(Site_ID,Date) %>%
  filter(meas_priority==min(meas_priority)) %>%
  add_tally()

table(DEC_priority$n) #Some sites still have more than one measurement per day. 
                      #The next few lines are to clean up those last few duplicates.

DEC_priority_clean <- bind_rows(DEC_priority %>%
                                  filter(n==1),
                                DEC_priority %>%
                                  filter(n>1) %>%
                                  filter(`NUM DAILY OBS`==max(`NUM DAILY OBS`))) %>%
  dplyr::select(-n) %>%
  add_tally()
table(DEC_priority_clean$n) 

DEC_priority_clean <- bind_rows(DEC_priority_clean %>%
                                  filter(n==1) %>%
                                  mutate(`RANKING NUMBER`=as.character(`RANKING NUMBER`),
                                         POC=as.character(POC)),
                                DEC_priority_clean %>%
                                  filter(n>1) %>%
                                  filter(POC_priority==0) %>%
                                  group_by( `# STATE CODE`,
                                            `COUNTY CODE`,
                                            `COUNTY NAME`,
                                            Site_No,
                                            `PARAMETER CODE`,    
                                            `DURATION CODE`,
                                            `DURATION DESC`,
                                            `UNIT CODE`,
                                            `UNIT DESC`,         
                                            `EDT ID`,
                                            `NUM DAILY OBS`,
                                            `PCT DAILY OBS`,
                                            `MAX HOUR`,
                                            `DAILY CRITERIA IND`,
                                            Site_ID,
                                            Date,
                                            POC_priority,
                                            meas_priority,n) %>%
                                  summarise(`ARITHMETIC MEAN`=mean(`ARITHMETIC MEAN`),
                                            `MAX VALUE`=max(`MAX VALUE`),
                                            `RANKING NUMBER`=paste(`RANKING NUMBER`,collapse="/"),
                                            POC=paste(POC,collapse="/"),
                                            .groups="drop"),
                                DEC_priority_clean %>%
                                  filter(n==2) %>%
                                  filter(POC_priority>0) %>% 
                                  dplyr::select(-`EDT ID`,
                                                -`RANKING NUMBER`) %>%
                                  distinct() %>%
                                  mutate(POC=as.character(POC))) %>%
  dplyr::select(-n) %>%
  group_by(Site_ID,Date) %>%
  add_tally()
table(DEC_priority_clean$n)

####Finish data cleaning for DEC measurements####
#'Average Anchorage site monitors
cleaned_DEC_data <- DEC_priority_clean %>%
  select(-n,-POC_priority,-meas_priority) %>%
  rename_at(vars(contains(".")),.funs=~gsub("\\."," ",.)) %>%
  rename(`STATE CODE`=`# STATE CODE`)

Site_1004_data <- cleaned_DEC_data %>%
  filter(Site_ID=="02-020-1004")
Anchorage_data <- cleaned_DEC_data %>%
  filter(`Site_ID`=="02-020-0018") %>%
  full_join(Site_1004_data,by=c("Date","COUNTY CODE","COUNTY NAME","STATE CODE","UNIT CODE","UNIT DESC"))
Site_No.x <- Anchorage_data$Site_No.x
Site_No.y <- Anchorage_data$Site_No.y

Anchorage_avg_data <- Anchorage_data %>%
  mutate_all(~replace(.,is.na(.),NA)) %>%
  rename_at(vars(contains(".x")),.funs=~gsub(".x",paste("_",unique(Site_No.x[which(!is.na(Site_No.x))]),sep=""),.)) %>%
  rename_at(vars(contains(".y")),.funs=~gsub(".y",paste("_",unique(Site_No.y[which(!is.na(Site_No.y))]),sep=""),.)) %>%
  group_by(Date) %>%
  mutate_at(vars(c(`Site_No_1004`,`Site_No_18`,`PARAMETER CODE_18`,`PARAMETER CODE_1004`,
                   `POC_1004`,`POC_18`,`DURATION CODE_1004`,`DURATION CODE_18`,
                   `DURATION DESC_1004`,`DURATION DESC_18`,`NUM DAILY OBS_1004`,`NUM DAILY OBS_18`,
                   `PCT DAILY OBS_1004`,`PCT DAILY OBS_18`,`DAILY CRITERIA IND_1004`,
                   `DAILY CRITERIA IND_18`,`EDT ID_1004`,`EDT ID_18`,`RANKING NUMBER_1004`,
                   `RANKING NUMBER_18`,Site_ID_1004,Site_ID_18)),
            .funs=funs(as.character)) %>%
  mutate(`ARITHMETIC MEAN`=mean(c(`ARITHMETIC MEAN_18`,`ARITHMETIC MEAN_1004`),na.rm=TRUE),
         `Site_No`=ifelse(is.na(`ARITHMETIC MEAN_18`),Site_No_1004,
                          ifelse(is.na(`ARITHMETIC MEAN_1004`),Site_No_18,paste(Site_No_1004,Site_No_18,sep="/"))),
         `Site_ID`=ifelse(is.na(`ARITHMETIC MEAN_18`),Site_ID_1004,
                          ifelse(is.na(`ARITHMETIC MEAN_1004`),Site_ID_18,paste(Site_ID_1004,Site_ID_18,sep="/"))),
         `PARAMETER CODE`=ifelse(is.na(`ARITHMETIC MEAN_18`),`PARAMETER CODE_1004`,
                                 ifelse(is.na(`ARITHMETIC MEAN_1004`),`PARAMETER CODE_18`,
                                        paste(`PARAMETER CODE_1004`,`PARAMETER CODE_18`,sep="/"))),
         `POC`=ifelse(is.na(`ARITHMETIC MEAN_18`),POC_1004,ifelse(is.na(`ARITHMETIC MEAN_1004`),POC_18,
                                                                  paste(POC_1004,POC_18,sep="/"))),
         `DURATION CODE`=ifelse(is.na(`ARITHMETIC MEAN_18`),`DURATION CODE_1004`,
                                ifelse(is.na(`ARITHMETIC MEAN_1004`),`DURATION CODE_18`,
                                       paste(`DURATION CODE_1004`,`DURATION CODE_18`,sep="/"))),
         `DURATION DESC`=ifelse(is.na(`ARITHMETIC MEAN_18`),`DURATION DESC_1004`,
                                ifelse(is.na(`ARITHMETIC MEAN_1004`),`DURATION DESC_18`,
                                       paste(`DURATION DESC_1004`,`DURATION DESC_18`,sep="/"))),
         `NUM DAILY OBS`=ifelse(is.na(`ARITHMETIC MEAN_18`),`NUM DAILY OBS_1004`,
                                ifelse(is.na(`ARITHMETIC MEAN_1004`),`NUM DAILY OBS_18`,
                                       paste(`NUM DAILY OBS_1004`,`NUM DAILY OBS_18`,sep="/"))),
         `PCT DAILY OBS`=ifelse(is.na(`ARITHMETIC MEAN_18`),`PCT DAILY OBS_1004`,
                                ifelse(is.na(`ARITHMETIC MEAN_1004`),`PCT DAILY OBS_18`,
                                       paste(`PCT DAILY OBS_1004`,`PCT DAILY OBS_18`,sep="/"))),
         `DAILY CRITERIA IND`=ifelse(is.na(`ARITHMETIC MEAN_18`),`DAILY CRITERIA IND_1004`,
                                     ifelse(is.na(`ARITHMETIC MEAN_1004`),`DAILY CRITERIA IND_18`,
                                            paste(`DAILY CRITERIA IND_1004`,`DAILY CRITERIA IND_18`,sep="/"))),
         `EDT ID`=ifelse(is.na(`ARITHMETIC MEAN_18`),`EDT ID_1004`,
                         ifelse(is.na(`ARITHMETIC MEAN_1004`),`EDT ID_18`,
                                paste(`EDT ID_1004`,`EDT ID_18`,sep="/"))),
         `RANKING NUMBER`=ifelse(is.na(`ARITHMETIC MEAN_18`),`RANKING NUMBER_1004`,
                                 ifelse(is.na(`ARITHMETIC MEAN_1004`),`RANKING NUMBER_18`,
                                        paste(`RANKING NUMBER_1004`,`RANKING NUMBER_18`,sep="/"))),
         `MAX VALUE`=max(c(`MAX VALUE_18`,`MAX VALUE_1004`),na.rm=TRUE),
         `MAX HOUR`=ifelse(max(c(`MAX VALUE_18`,`MAX VALUE_1004`),na.rm=TRUE)==`MAX VALUE_18`,`MAX HOUR_18`,
                           ifelse(max(`MAX VALUE_18`,`MAX VALUE_1004`,na.rm=TRUE)==`MAX VALUE_1004`,`MAX HOUR_1004`,NA))) %>%
  select(-contains("_18"),-contains("_1004")) %>%
  filter(!is.na(Site_No))

avg_DEC_data <- cleaned_DEC_data %>%
  ungroup() %>%
  mutate_at(vars(c(`Site_No`,`PARAMETER CODE`,`POC`,`DURATION CODE`,`DURATION DESC`,
                   `NUM DAILY OBS`,`PCT DAILY OBS`,`DAILY CRITERIA IND`,`EDT ID`,
                   `RANKING NUMBER`,Site_ID)),
            .funs=funs(as.character)) %>%
  filter(`COUNTY NAME`!="Anchorage") %>%
  bind_rows(Anchorage_avg_data)

#' Add in long/lat for monitor sites
library(geosphere)
midPoint(c(-149.82460,61.205861),c(-149.56972,61.32669)) # -149.6974 61.26634
long_key <- c("02-020-0018" = "-149.82460", "02-170-0008" = "-149.031655",
              "02-020-1004" = "-149.56972", "02-090-0034" = "-147.72727",
              "02-020-1004/02-020-0018" = "-149.6974")
lat_key <- c("02-020-0018" = "61.205861", "02-170-0008" = "61.534163",
             "02-020-1004" = "61.32669", "02-090-0034" = "64.8458",
             "02-020-1004/02-020-0018" = "61.26634")
Longitude <- avg_DEC_data$Site_ID
Longitude <- recode(Longitude,!!!long_key)

Latitude <- avg_DEC_data$Site_ID
Latitude <- recode(Latitude,!!!lat_key)

avg_DEC_data$PM_Longitude <- Longitude
avg_DEC_data$PM_Latitude <- Latitude

#'Here, the dataset is complete, so a .csv file will be saved with the single daily
#'measurements.
write.csv(avg_DEC_data,'../clean_data/DEC_data.csv')
