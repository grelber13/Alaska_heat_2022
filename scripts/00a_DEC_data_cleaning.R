## ---------------------------
## Script name: DEC data cleaning
## Author: Grace Kuiper
## Version: 06/15/2020

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
DEC_data <- dplyr::rename(DEC_data,Site_No=`SITE ID`)
for (i in 1:nrow(DEC_data)) {
  if (DEC_data[i,"COUNTY CODE"]=="20" & DEC_data[i,"Site_No"]=="18") {
    DEC_data[i,"Site_ID"] <- "02-020-0018"
  } else if (DEC_data[i,"COUNTY CODE"]=="20" & DEC_data[i,"Site_No"]=="1004") {
    DEC_data[i,"Site_ID"] <- "02-020-1004"
  } else if (DEC_data[i,"COUNTY CODE"]=="170") {
    DEC_data[i,"Site_ID"] <- "02-170-0008"
  } else if (DEC_data[i,"COUNTY CODE"]=="90") {
    DEC_data[i,"Site_ID"] <- "02-090-0034"
  }
}

#'Format date variable
DEC_data <- DEC_data %>%
  mutate(Date=as.Date(as.character(`COLLECTION DATE`),tryFormats = c("%Y%m%d"))) %>%
  select(-`COLLECTION DATE`,-`YEAR`,-`MONTH`,-`DAY`)

#'Eliminate readings with fewer than 75% of measurements (18 hours) for summarized hourly data
DEC_data <- DEC_data %>%
  filter(`PCT DAILY OBS`>=75) %>%
  filter(ifelse(`DURATION DESC`=="1 HOUR",`NUM DAILY OBS`>=18,`NUM DAILY OBS`>0)) %>%
  filter(`ARITHMETIC MEAN`>0)

#'Number readings for each date and each site
DEC_data <- DEC_data %>%
  group_by(Site_ID,Date) %>%
  add_tally()

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
for (i in 1:nrow(DEC_data)) {
  if (DEC_data[i,"Site_ID"]=="02-020-0018" & DEC_data[i,"POC"]==3) {
    DEC_data[i,"POC_priority"]<-1
  } else if (DEC_data[i,"Site_ID"]=="02-020-1004" & DEC_data[i,"POC"]==3) {
    DEC_data[i,"POC_priority"]<-1
  } else if (DEC_data[i,"Site_ID"]=="02-170-0008") {
    if (DEC_data[i,"POC"]==3) {
      DEC_data[i,"POC_priority"]<-1
    } else if (DEC_data[i,"POC"]==1) {
      DEC_data[i,"POC_priority"]<-2
    }
  } else if (DEC_data[i,"Site_ID"]=="02-090-0034") {
    if (DEC_data[i,"POC"]==1) {
      DEC_data[i,"POC_priority"]<-1
    } else if (DEC_data[i,"POC"]==2) {
      DEC_data[i,"POC_priority"]<-2
    } else if (DEC_data[i,"POC"]==3) {
      DEC_data[i,"POC_priority"]<-3
    }
  }
}
DEC_data <- DEC_data %>% replace_na(list(POC_priority = 0))

####Single daily measurements####
#'Extract days/sites that only have one measurement
single_readings <- DEC_data %>%
  filter(n==1)

####Two daily measurements####
#'Next, extract days/sites that have two measurements
double_readings <- DEC_data %>%
  filter(n==2)

#'Select the primary monitor for duplicate measurements
double_readings_prim <- double_readings %>%
  filter(POC_priority==1) %>%
  select(-n) %>%
  group_by(Site_ID,Date) %>%
  add_tally()

single_readings <- double_readings_prim %>%
  filter(n==1) %>%
  bind_rows(single_readings)

double_readings_prim <- double_readings_prim %>%
  filter(n==2)

#'For priority #1 sites, select 1-hour measures over 24-hour averages
double_readings_prim_hour <- double_readings_prim %>%
  filter(`DURATION DESC`=="1 HOUR") %>%
  select(-n) %>%
  group_by(Site_ID,Date) %>%
  add_tally()

single_readings <- double_readings_prim_hour %>%
  filter(n==1) %>%
  bind_rows(single_readings)

#'Now deal with double measurements with no first priority; no priority #2 sites
#'were included in double measurements. For priority #3 sites, select 1-hour measures
#'over 24-hour averages.
double_readings_tert <- double_readings %>%
  filter(POC_priority==3) %>%
  select(-n) %>%
  group_by(Site_ID,Date) %>%
  add_tally() %>%
  filter(n==2)

double_readings_tert_hour <- double_readings_tert %>%
  filter(n==2) %>%
  filter(`DURATION DESC`=="1 HOUR") %>%
  select(-n) %>%
  group_by(Site_ID,Date) %>%
  add_tally()

single_readings <- double_readings_tert_hour %>%
  filter(n==1) %>%
  bind_rows(single_readings)

#'Finally, average the double readings for sites with no assigned priority
double_readings_zero <- double_readings %>%
  filter(POC_priority==0) %>%
  select(-n) %>%
  group_by(Site_ID,Date) %>%
  add_tally() %>%
  filter(n==2)

double_readings_zero <- double_readings_zero %>%
  select(-n,-POC,-`RANKING NUMBER`) %>%
  group_by(`# STATE CODE`,`COUNTY CODE`,`COUNTY NAME`,Site_No,
           `PARAMETER CODE`,`DURATION CODE`,`DURATION DESC`,`UNIT CODE`,
           `UNIT DESC`,`EDT ID`,`DAILY CRITERIA IND`,Site_ID,Date,
           POC_priority) %>%
  summarise_all(list(mean = mean)) %>%
  mutate(POC="1/2",
         `RANKING NUMBER`="102/22") %>%
  add_tally() %>%
  rename_at(vars(contains("_mean")),funs(gsub("_mean","",.)))

single_readings <- single_readings %>%
  mutate(POC=as.character(POC),
         `RANKING NUMBER`=as.character(`RANKING NUMBER`)) %>%
  bind_rows(double_readings_zero)

####Three daily measurements####
#'Next, pull out days/sites that have three measurements
triple_readings <- DEC_data %>%
  filter(n==3)

#'Select the primary monitor for triple measurements
triple_readings_prim <- triple_readings %>%
  filter(POC_priority==1) %>%
  select(-n) %>%
  group_by(Site_ID,Date) %>%
  add_tally()

single_readings <- triple_readings_prim %>%
  mutate(POC=as.character(POC),
         `RANKING NUMBER`=as.character(`RANKING NUMBER`)) %>%
  filter(n==1) %>%
  bind_rows(single_readings)

triple_readings_prim <- triple_readings_prim %>%
  filter(n!=1)

#For priority #1 sites, select 1-hour measures over 24-hour averages
triple_readings_prim_hour <- triple_readings_prim %>%
  filter(`DURATION DESC`=="1 HOUR") %>%
  select(-n) %>%
  group_by(Site_ID,Date) %>%
  add_tally()

single_readings <- triple_readings_prim_hour %>%
  mutate(POC=as.character(POC),
         `RANKING NUMBER`=as.character(`RANKING NUMBER`)) %>%
  filter(n==1) %>%
  bind_rows(single_readings)

#'Now deal with triple measurements with no first priority
triple_readings_no_prim <- triple_readings %>%
  filter(POC_priority!=1) %>%
  select(-n) %>%
  group_by(Site_ID,Date) %>%
  add_tally() %>%
  filter(n==3)

#'Next add priority #2 sites
triple_readings_sec <- triple_readings_no_prim %>%
  filter(POC_priority==2) %>%
  select(-n) %>%
  group_by(Site_ID,Date) %>%
  add_tally()

single_readings <- triple_readings_sec %>%
  mutate(POC=as.character(POC),
         `RANKING NUMBER`=as.character(`RANKING NUMBER`)) %>%
  filter(n==1) %>%
  bind_rows(single_readings)

#'Next add priority #3 sites
triple_readings_no_sec <- triple_readings_no_prim %>%
  filter(POC_priority!=2) %>%
  select(-n) %>%
  group_by(Site_ID,Date) %>%
  add_tally() %>%
  filter(n==3)

triple_readings_tert <- triple_readings_no_sec %>%
  filter(POC_priority==3) %>%
  select(-n) %>%
  group_by(Site_ID,Date) %>%
  add_tally()

#For priority #3 sites, select 1-hour measures over 24-hour averages
triple_readings_tert_hour <- triple_readings_tert %>%
  filter(`DURATION DESC`=="1 HOUR") %>%
  select(-n) %>%
  group_by(Site_ID,Date) %>%
  add_tally()

single_readings <- triple_readings_tert_hour %>%
  mutate(POC=as.character(POC),
         `RANKING NUMBER`=as.character(`RANKING NUMBER`)) %>%
  filter(n==1) %>%
  bind_rows(single_readings)

####Four daily measurements####
#'Next, pull out days/sites that have four measurements
quadruple_readings <- DEC_data %>%
  filter(n==4)

#'Select readings from the primary monitor and remove duplicate measures that only 
#'differ by `RANKING NUMBER` and `EDT ID`.
quadruple_readings_prim <- quadruple_readings %>%
  distinct(`# STATE CODE`,`COUNTY CODE`,`COUNTY NAME`,`Site_No`,
           `PARAMETER CODE`,`POC`,`DURATION CODE`,`DURATION DESC`,
           `UNIT CODE`,`UNIT DESC`,`ARITHMETIC MEAN`,`NUM DAILY OBS`,
           `PCT DAILY OBS`,`MAX HOUR`,`MAX VALUE`,`DAILY CRITERIA IND`,
           `Site_ID`,`Date`,`POC_priority`,`n`) %>%
  filter(POC_priority==1) %>%
  select(-n) %>%
  group_by(Site_ID,Date) %>%
  add_tally()

single_readings <- quadruple_readings_prim %>%
  mutate(POC=as.character(POC)) %>%
  filter(n==1) %>%
  bind_rows(single_readings)

quadruple_readings_prim <- quadruple_readings_prim %>%
  filter(n!=1) 

#'For priority #1 sites, select 1-hour measures over 24-hour averages
quadruple_readings_prim_hour <- quadruple_readings_prim %>%
  filter(`DURATION DESC`=="1 HOUR") %>%
  select(-n) %>%
  group_by(Site_ID,Date) %>%
  add_tally()

single_readings <- quadruple_readings_prim_hour %>%
  mutate(POC=as.character(POC)) %>%
  filter(n==1) %>%
  bind_rows(single_readings)

#'Now deal with quadruple measurements with no first priority
quadruple_readings_no_prim <- quadruple_readings %>%
  filter(POC_priority!=1) %>%
  select(-n) %>%
  group_by(Site_ID,Date) %>%
  add_tally() %>%
  filter(n==4)

#'Next add priority #2 readings
quadruple_readings_sec <- quadruple_readings_no_prim %>%
  filter(POC_priority==2) %>%
  select(-n) %>%
  group_by(Site_ID,Date) %>%
  add_tally()

single_readings <- quadruple_readings_sec %>%
  distinct(`# STATE CODE`,`COUNTY CODE`,`COUNTY NAME`,`Site_No`,
           `PARAMETER CODE`,`POC`,`DURATION CODE`,`DURATION DESC`,
           `UNIT CODE`,`UNIT DESC`,`ARITHMETIC MEAN`,`NUM DAILY OBS`,
           `PCT DAILY OBS`,`MAX HOUR`,`MAX VALUE`,`DAILY CRITERIA IND`,
           `Site_ID`,`Date`,`POC_priority`,`n`) %>%
  mutate(POC=as.character(POC)) %>%
  select(-n) %>%
  group_by(Site_ID,Date) %>%
  add_tally() %>%
  filter(n==1) %>%
  bind_rows(single_readings)

#'Next, add priority #3 readings
quadruple_readings_tert <- quadruple_readings_no_prim %>%
  filter(POC_priority!=2) %>%
  select(-n) %>%
  group_by(Site_ID,Date) %>%
  add_tally() %>%
  filter(n==4)

#'For priority #3 sites, select 1-hour measures over 24-hour averages
quadruple_readings_tert_hour <- quadruple_readings_tert %>%
  distinct(`# STATE CODE`,`COUNTY CODE`,`COUNTY NAME`,`Site_No`,
           `PARAMETER CODE`,`POC`,`DURATION CODE`,`DURATION DESC`,
           `UNIT CODE`,`UNIT DESC`,`ARITHMETIC MEAN`,`NUM DAILY OBS`,
           `PCT DAILY OBS`,`MAX HOUR`,`MAX VALUE`,`DAILY CRITERIA IND`,
           `Site_ID`,`Date`,`POC_priority`,`n`) %>%
  filter(`DURATION DESC`=="1 HOUR") %>%
  select(-n) %>%
  group_by(Site_ID,Date) %>%
  add_tally()

single_readings <- quadruple_readings_tert_hour %>%
  mutate(POC=as.character(POC)) %>%
  filter(n==1) %>%
  bind_rows(single_readings)

####Five daily measurements####
#'Next, pull out days/sites that have five measurements
quintuple_readings <- DEC_data %>%
  filter(n==5)

#'Select readings from the primary monitor
quintuple_readings_prim <- quintuple_readings %>%
  filter(POC_priority==1) %>%
  distinct(`# STATE CODE`,`COUNTY CODE`,`COUNTY NAME`,`Site_No`,
           `PARAMETER CODE`,`POC`,`DURATION CODE`,`DURATION DESC`,
           `UNIT CODE`,`UNIT DESC`,`ARITHMETIC MEAN`,`NUM DAILY OBS`,
           `PCT DAILY OBS`,`MAX HOUR`,`MAX VALUE`,`DAILY CRITERIA IND`,
           `Site_ID`,`Date`,`POC_priority`,`n`) %>%
  select(-n) %>%
  group_by(Site_ID,Date) %>%
  add_tally()

single_readings <- quintuple_readings_prim %>%
  mutate(POC=as.character(POC)) %>%
  filter(n==1) %>%
  bind_rows(single_readings)

#'Now deal with quintuple measurements with no first priority; there are no
#'priority #2 readings for quintuple measurements
quintuple_readings_tert <- quintuple_readings %>%
  filter(POC_priority!=1) %>%
  select(-n) %>%
  group_by(Site_ID,Date) %>%
  add_tally() %>%
  filter(n==5)

#'For priority #3 sites, select 1-hour measures over 24-hour averages
quintuple_readings_tert_hour <- quintuple_readings_tert %>%
  distinct(`# STATE CODE`,`COUNTY CODE`,`COUNTY NAME`,`Site_No`,
           `PARAMETER CODE`,`POC`,`DURATION CODE`,`DURATION DESC`,
           `UNIT CODE`,`UNIT DESC`,`ARITHMETIC MEAN`,`NUM DAILY OBS`,
           `PCT DAILY OBS`,`MAX HOUR`,`MAX VALUE`,`DAILY CRITERIA IND`,
           `Site_ID`,`Date`,`POC_priority`,`n`) %>%
  filter(`DURATION DESC`=="1 HOUR") %>%
  select(-n) %>%
  group_by(Site_ID,Date) %>%
  add_tally()

single_readings <- quintuple_readings_tert_hour %>%
  mutate(POC=as.character(POC)) %>%
  filter(n==1) %>%
  bind_rows(single_readings)

####Six daily measurements####
#'Next, pull out days/sites that have six measurements
sextuple_readings <- DEC_data %>%
  filter(n==6)

#'Select readings from the primary monitor
sextuple_readings_prim <- sextuple_readings %>%
  filter(POC_priority==1) %>%
  distinct(`# STATE CODE`,`COUNTY CODE`,`COUNTY NAME`,`Site_No`,
           `PARAMETER CODE`,`POC`,`DURATION CODE`,`DURATION DESC`,
           `UNIT CODE`,`UNIT DESC`,`ARITHMETIC MEAN`,`NUM DAILY OBS`,
           `PCT DAILY OBS`,`MAX HOUR`,`MAX VALUE`,`DAILY CRITERIA IND`,
           `Site_ID`,`Date`,`POC_priority`,`n`) %>%
  select(-n) %>%
  group_by(Site_ID,Date) %>%
  add_tally()

single_readings <- sextuple_readings_prim %>%
  mutate(POC=as.character(POC)) %>%
  filter(n==1) %>%
  bind_rows(single_readings)

#'Now deal with quadruple measurements with no first priority; there are no
#'priority #2 readings for sextuple measurements
sextuple_readings_tert <- sextuple_readings %>%
  filter(POC_priority!=1) %>%
  select(-n) %>%
  group_by(Site_ID,Date) %>%
  add_tally() %>%
  filter(n==6)

sextuple_readings_tert <- sextuple_readings_tert %>%
  distinct(`# STATE CODE`,`COUNTY CODE`,`COUNTY NAME`,`Site_No`,
           `PARAMETER CODE`,`POC`,`DURATION CODE`,`DURATION DESC`,
           `UNIT CODE`,`UNIT DESC`,`ARITHMETIC MEAN`,`NUM DAILY OBS`,
           `PCT DAILY OBS`,`MAX HOUR`,`MAX VALUE`,`DAILY CRITERIA IND`,
           `Site_ID`,`Date`,`POC_priority`,`n`) %>%
  select(-n) %>%
  group_by(Site_ID,Date) %>%
  add_tally()

#'For priority #3 sites, select 1-hour measures over 24-hour averages
sextuple_readings_tert_hour <- sextuple_readings_tert %>%
  filter(`DURATION DESC`=="1 HOUR") %>%
  arrange(Date,Site_ID,-`NUM DAILY OBS`) %>%
  select(-n) %>%
  group_by(Site_ID,Date) %>%
  add_tally()

sextuple_readings_tert_hour <- sextuple_readings_tert_hour %>%
  dplyr::arrange(Date,desc(`NUM DAILY OBS`)) %>%
  distinct(Date,.keep_all=TRUE) %>%
  select(-n) %>%
  group_by(Site_ID,Date) %>%
  add_tally()

single_readings <- sextuple_readings_tert_hour %>%
  mutate(POC=as.character(POC)) %>%
  filter(n==1) %>%
  bind_rows(single_readings)

####Seven daily measurements####
#'Next, pull out days/sites that have seven measurements
heptuple_readings <- DEC_data %>%
  filter(n==7)

heptuple_readings_prim <- heptuple_readings %>%
  filter(POC_priority==1) %>%
  distinct(`# STATE CODE`,`COUNTY CODE`,`COUNTY NAME`,`Site_No`,
           `PARAMETER CODE`,`POC`,`DURATION CODE`,`DURATION DESC`,
           `UNIT CODE`,`UNIT DESC`,`ARITHMETIC MEAN`,`NUM DAILY OBS`,
           `PCT DAILY OBS`,`MAX HOUR`,`MAX VALUE`,`DAILY CRITERIA IND`,
           `Site_ID`,`Date`,`POC_priority`,`n`) %>%
  select(-n) %>%
  group_by(Site_ID,Date) %>%
  add_tally()

single_readings <- heptuple_readings_prim %>%
  mutate(POC=as.character(POC)) %>%
  filter(n==1) %>%
  bind_rows(single_readings)

####Eight daily measurements####
#'Next, pull out days/sites that have eight measurements
octuple_readings <- DEC_data %>%
  filter(n==8)

octuple_readings_prim <- octuple_readings %>%
  filter(POC_priority==1) %>%
  distinct(`# STATE CODE`,`COUNTY CODE`,`COUNTY NAME`,`Site_No`,
           `PARAMETER CODE`,`POC`,`DURATION CODE`,`DURATION DESC`,
           `UNIT CODE`,`UNIT DESC`,`ARITHMETIC MEAN`,`NUM DAILY OBS`,
           `PCT DAILY OBS`,`MAX HOUR`,`MAX VALUE`,`DAILY CRITERIA IND`,
           `Site_ID`,`Date`,`POC_priority`,`n`) %>%
  select(-n) %>%
  group_by(Site_ID,Date) %>%
  add_tally()

single_readings <- octuple_readings_prim %>%
  mutate(POC=as.character(POC)) %>%
  filter(n==1) %>%
  bind_rows(single_readings)

####Nine daily measurements####
#'Next, pull out days/sites that have nine measurements
nontuple_readings <- DEC_data %>%
  filter(n==9)

nontuple_readings_prim <- nontuple_readings %>%
  filter(POC_priority==1) %>%
  distinct(`# STATE CODE`,`COUNTY CODE`,`COUNTY NAME`,`Site_No`,
           `PARAMETER CODE`,`POC`,`DURATION CODE`,`DURATION DESC`,
           `UNIT CODE`,`UNIT DESC`,`ARITHMETIC MEAN`,`NUM DAILY OBS`,
           `PCT DAILY OBS`,`MAX HOUR`,`MAX VALUE`,`DAILY CRITERIA IND`,
           `Site_ID`,`Date`,`POC_priority`,`n`) %>%
  select(-n) %>%
  group_by(Site_ID,Date) %>%
  add_tally()

single_readings <- nontuple_readings_prim %>%
  mutate(POC=as.character(POC)) %>%
  filter(n==1) %>%
  bind_rows(single_readings)

single_readings <- single_readings %>%
  arrange(Date)

#'There were no sites/days with more than nine readings.

####Finish data cleaning for DEC measurements####
#'Average Anchorage site monitors
cleaned_DEC_data <- single_readings %>%
  select(-n,-POC_priority) %>%
  rename_at(vars(contains(".")),.funs=~gsub("\\."," ",.)) %>%
  rename(`STATE CODE`=`X  STATE CODE`)

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
