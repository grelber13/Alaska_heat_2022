## ---------------------------
## Script name: HFDR health data cleaning
## Author: Grace Kuiper
## Version: 08/06/2020

## Purpose: Clean raw HDFR data to be usable for future analyses.

## Email: grace.kuiper@colostate.edu
## ---------------------------

library(tidyverse)
library(data.table)
library(lubridate)
library(eeptools)
library(dplyr)
library(readxl)

#'Import HFDR data
health_data<-read.delim('../raw_data/ipopdd2015v9_2019v1_ichs.txt', header = TRUE, sep = "\t", dec = ".") 
hospital_data <- read.delim('../raw_data/hospital.txt', header = TRUE, sep= "\t", dec = ".")
state_county_codes <- c(2020,2170,2090)
rel_hospital_ids <-hospital_data %>%
  filter(state_county %in% state_county_codes) %>%
  select(medicare_provider_no)
rel_hospital_info <- hospital_data %>%
  filter(state_county %in% state_county_codes)
rel_hospital_ids <- rel_hospital_ids$medicare_provider_no

health_data <- health_data %>%
  filter(facilityid %in% rel_hospital_ids) %>%
  mutate(Date=as.Date(admitdate,tryFormats=c("%m/%d/%Y")),
         county=ifelse(facilityregion=="Anchorage","Anchorage",
                       ifelse(facilityregion=="Matanuska-Susitna","Matanuska-Susitna",
                              "Fairbanks North Star"))) 
HFDR_data <- health_data %>%
  filter(grepl("-06-|-07-|-08-",Date)) %>%
  filter(Date>"2015-01-01")

#'Recode binary health variables with primary diagnosis only
asthma_dx <- c("493","J45")
copd_dx <- c("490","491","492","494","496","J40","J41","J42","J43","J44","J47")
pneumonia_dx <- c("480","481","482","483","484","485","486","J12","J13","J14","J15","J16","J17","J18")
bronchitis_dx <- c("466","J20","J21","J22")
respiratory_dx <- c(asthma_dx,copd_dx,pneumonia_dx,bronchitis_dx)

arrythmia_dx <- c("427","I46","I47","I48","I49")
cerebrovascular_dx <- c("430","431","432","433","434","435","436","437","438",
                        "I60","I61","I62","I63","I65","I66","I67","I68","I69","G45","I23")
heartfailure_dx <- c("428","I50")
ischemic_dx <- c("410","411","412","413","414","I20","I21","I22","I24","I25")
myocardial_dx <- c("410","I21","I22")
cardiovascular_dx <- c(arrythmia_dx,cerebrovascular_dx,heartfailure_dx,ischemic_dx)

heatillness_dx <- c("992","T67")
forearmfracture_dx <- c("813","S52")

cardiorespiratory_dx <- c(respiratory_dx,cardiovascular_dx)

HFDR_data <- HFDR_data %>%
  select(-21:-49,-53:-65) %>%
  mutate(prindx_sub=substr(prindx,1,3)) %>%
  mutate(asthma=ifelse(prindx_sub %in% asthma_dx,1,0),
         copd=ifelse(prindx_sub %in% copd_dx,1,0),
         pneumonia=ifelse(prindx_sub %in% pneumonia_dx,1,0),
         bronchitis=ifelse(prindx_sub %in% bronchitis_dx,1,0),
         respiratory=ifelse(prindx_sub %in% respiratory_dx,1,0),
         arrythmia=ifelse(prindx_sub %in% arrythmia_dx,1,0),
         cerebrovascular=ifelse(prindx_sub %in% cerebrovascular_dx,1,0),
         heartfailure=ifelse(prindx_sub %in% heartfailure_dx,1,0),
         ischemic=ifelse(prindx_sub %in% ischemic_dx,1,0),
         myocardial=ifelse(prindx_sub %in% myocardial_dx,1,0),
         cardiovascular=ifelse(prindx_sub %in% cardiovascular_dx,1,0),
         heatillness=ifelse(prindx_sub %in% heatillness_dx,1,0),
         forearmfracture=ifelse(prindx_sub %in% forearmfracture_dx,1,0),
         cardiorespiratory=ifelse(prindx_sub %in% cardiorespiratory_dx,1,0)) %>%
  filter(enctype==2) %>%
  filter(placesvc==1) %>%
  distinct(hfdrepisode,.keep_all=TRUE)

HFDR_cardioresp <- HFDR_data %>%
  filter(cardiorespiratory==1) 
HFDR_forearmfracture <- HFDR_data %>%
  filter(forearmfracture==1)
HFDR_heatillness <- HFDR_data %>%
  filter(heatillness==1) 

HFDR_data <- bind_rows(HFDR_cardioresp,HFDR_forearmfracture,HFDR_heatillness)

#Define a lag function
funlag <- function(var, n=6){
  var <- enquo(var)
  indices <- seq_len(n)
  map( indices, ~quo(lag(!!var, !!.x)) ) %>% 
    set_names(sprintf("%s_lag%d", rlang::quo_text(var), indices))
}

exp_data <- exp_data %>%
  group_by(county) %>%
  mutate(., !!!funlag(mean_relh,5),!!!funlag(mean_tmpf,5), 
         !!!funlag(max_tmpf,5),!!!funlag(ARITHMETIC.MEAN,5))

#'Create case-crossover dataset
dates <- HFDR_data[,c("Date","county","hfdrepisode","race","age","sex","ethnicity")]

dates <- dates[rep(seq_len(nrow(dates)), each = 53), ]
dates$index <- rep(-26:26,nrow(dates)/53)
ref_dates <- dates %>%
  mutate(Date=Date+(index*7)) %>%
  filter(grepl("-06-|-07-|-08-",Date)) %>%
  filter(Date>"2015-01-01")
ref_dates <- ref_dates %>%
  filter(index!=0) %>%
  select(-index)

replaceNA <- function(x) (ifelse(is.na(x),0,x))
HFDR_full <- bind_rows(HFDR_data,ref_dates) %>%
  arrange(county,Date) %>%
  mutate_at(c("asthma","copd","pneumonia","bronchitis","respiratory","arrythmia",
              "cerebrovascular","heartfailure","ischemic","myocardial","cardiovascular",
              "heatillness","forearmfracture","cardiorespiratory"),replaceNA)

full_dataset <- left_join(HFDR_full,exp_data,by=c("county","Date","quarter"))
write_csv(full_dataset,"../clean_data/full_HFDR_dataset.csv")

#'Create list of casecrossover dataframes
asthma_full <- full_dataset %>%
  group_by(hfdrepisode) %>%
  mutate(n=length(unique(asthma))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=asthma) %>%
  mutate(out_name="asthma")
copd_full <- full_dataset %>%
  group_by(hfdrepisode) %>%
  mutate(n=length(unique(copd))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=copd) %>%
  mutate(out_name="copd")
pneumonia_full <- full_dataset %>%
  group_by(hfdrepisode) %>%
  mutate(n=length(unique(pneumonia))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=pneumonia) %>%
  mutate(out_name="pneumonia")
bronchitis_full <- full_dataset %>%
  group_by(hfdrepisode) %>%
  mutate(n=length(unique(bronchitis))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=bronchitis) %>%
  mutate(out_name="bronchitis")
respiratory_full <- full_dataset %>%
  group_by(hfdrepisode) %>%
  mutate(n=length(unique(respiratory))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=respiratory) %>%
  mutate(out_name="respiratory")
arrhythmia_full <- full_dataset %>%
  group_by(hfdrepisode) %>%
  mutate(n=length(unique(arrythmia))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=arrythmia) %>%
  mutate(out_name="arrhythmia")
cerebrovascular_full <- full_dataset %>%
  group_by(hfdrepisode) %>%
  mutate(n=length(unique(cerebrovascular))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=cerebrovascular) %>%
  mutate(out_name="cerebrovascular")
heartfailure_full <- full_dataset %>%
  group_by(hfdrepisode) %>%
  mutate(n=length(unique(heartfailure))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=heartfailure) %>%
  mutate(out_name="heartfailure")
ischemic_full <- full_dataset %>%
  group_by(hfdrepisode) %>%
  mutate(n=length(unique(ischemic))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=ischemic) %>%
  mutate(out_name="ischemic")
myocardial_full <- full_dataset %>%
  group_by(hfdrepisode) %>%
  mutate(n=length(unique(myocardial))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=myocardial) %>%
  mutate(out_name="myocardial")
cardiovascular_full <- full_dataset %>%
  group_by(hfdrepisode) %>%
  mutate(n=length(unique(cardiovascular))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=cardiovascular) %>%
  mutate(out_name="cardiovascular")
heatillness_full <- full_dataset %>%
  group_by(hfdrepisode) %>%
  mutate(n=length(unique(heatillness))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=heatillness) %>%
  mutate(out_name="heatillness")
forearmfracture_full <- full_dataset %>%
  group_by(hfdrepisode) %>%
  mutate(n=length(unique(forearmfracture))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=forearmfracture) %>%
  mutate(out_name="forearmfracture")
cardiorespiratory_full <- full_dataset %>%
  group_by(hfdrepisode) %>%
  mutate(n=length(unique(cardiorespiratory))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=cardiorespiratory) %>%
  mutate(out_name="cardiorespiratory")

casecross_list <- list(asthma_full,copd_full,pneumonia_full,bronchitis_full,
                       respiratory_full,arrhythmia_full,cerebrovascular_full,
                       ischemic_full,myocardial_full,heartfailure_full,
                       cardiovascular_full,heatillness_full,forearmfracture_full,
                       cardiorespiratory_full)

saveRDS(casecross_list,"../clean_data/casecross_list.rds")
