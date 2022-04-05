## ---------------------------
## Script name: Acute and prolonged heatwave analyses among overall study population
## Author: Grace Kuiper
## Version: 03/29/2022

## Purpose: For the purpose of AK heat analysis using heat index based on maximum daily 
##          temperature: evaluate the impact of prolonged elevated heat on the risk of 
##          cardiorespiratory- and heat illness-related emergency department visits 
##          using two approaches that employ conditional logistic regression models.

## Email: grace.kuiper@colostate.edu
## ---------------------------

library(tidyverse) 
library(survival) 
library(splines)
library(lubridate)
library(broom)
library(stringr)
library(dlnm)
library(dplyr,warn.conflicts = F)
library(xlsx)

#'Read in finalized exposure dataset
final_exp_data <- read.csv('../clean_data/final_exp_data.csv') %>%
  select(-X)

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

#'For each threshold, define "hot days" (i.e., days above the HI threshold)
thresholds <- c(seq(70,90,2))
hot_days <- data.frame()
for (i in 1:length(thresholds)) {
  hot_days_temp <- final_exp_data %>%
    ungroup() %>%
    select(HI,COUNTY.NAME,date) %>%
    filter(HI>thresholds[i]) %>%
    select(-HI) %>%
    mutate(threshold=thresholds[i])
  hot_days <- rbind(hot_days,hot_days_temp)
}
hot_days <- hot_days %>%
  mutate(hot_day=1)

#'Create dataframe of all dates and study sites in the study and merge with
#'identified "hot days".
counties <- c("Anchorage","Fairbanks North Star","Matanuska-Susitna")
counties_rep <- rep(rep(c("Anchorage","Fairbanks North Star","Matanuska-Susitna"),each=nrow(final_exp_data)/3),length(thresholds))
dates_rep <- rep(unique(final_exp_data$date),length(thresholds)*3)
thresholds_rep <- rep(c(thresholds),each=nrow(final_exp_data))
all_days_thresholds_df <- data.frame(COUNTY.NAME=counties_rep,
                                     date=dates_rep,
                                     threshold=thresholds_rep)

all_days_thresholds_df <- all_days_thresholds_df %>%
  left_join(hot_days,
             by=c("COUNTY.NAME","date","threshold")) %>%
  mutate(hot_day=ifelse(is.na(hot_day),0,hot_day))

#'For the prolonged heatwave analysis, define a linear variable of the number of
#'previous consecutive "hot days" (`previous_heatwave`).
#'For the acute heatwave analysis, define a binary variable of `1` for the same-day
#'and previous day both being above the HI threshold or `0` for the same-day and/or
#'previous day being below the HI threshold (`heatwave_day`)
for (h in 1:length(counties)) {
  for (j in 1:length(thresholds)) {  
    temp_heatwave_days <- all_days_thresholds_df %>%
      filter(COUNTY.NAME==counties[h]) %>%
      filter(threshold==thresholds[j]) %>%
      arrange(date) %>%
      mutate(previous_heatwave=0)
    if (h==1 & j==1) {
      for (i in 2:nrow(temp_heatwave_days)) {
        if (temp_heatwave_days[i-1,"hot_day"]==0) {
          temp_heatwave_days[i,"previous_heatwave"]<-0
        } else {
          temp_heatwave_days[i,"previous_heatwave"]<-temp_heatwave_days[i-1,"previous_heatwave"]+temp_heatwave_days[i-1,"hot_day"]
        }
      }
      for (i in 2:nrow(temp_heatwave_days)) {
        if (temp_heatwave_days[i-1,"hot_day"]==1 & temp_heatwave_days[i,"hot_day"]==1) {
          temp_heatwave_days[i,"heatwave_day"] <- 1
        } else {
          temp_heatwave_days[i,"heatwave_day"] <- 0
        }
      }
      heatwave_days <- temp_heatwave_days %>%
        select(COUNTY.NAME,date,hot_day,previous_heatwave,heatwave_day,threshold)
    } else {
      for (i in 2:nrow(temp_heatwave_days)) {
        if (temp_heatwave_days[i-1,"hot_day"]==0) {
          temp_heatwave_days[i,"previous_heatwave"] <- 0
        } else {
          temp_heatwave_days[i,"previous_heatwave"]<-temp_heatwave_days[i-1,"previous_heatwave"]+temp_heatwave_days[i-1,"hot_day"]
        }
      }
      for (i in 2:nrow(temp_heatwave_days)) {
        if (temp_heatwave_days[i-1,"hot_day"]==1 & temp_heatwave_days[i,"hot_day"]==1) {
          temp_heatwave_days[i,"heatwave_day"] <- 1
        } else {
          temp_heatwave_days[i,"heatwave_day"] <- 0
        }
      }
      heatwave_days <- bind_rows(heatwave_days,temp_heatwave_days %>%
                                   select(COUNTY.NAME,date,hot_day,previous_heatwave,heatwave_day,threshold))
    }
  }
}

heatwave_days <- heatwave_days %>%
  pivot_wider(names_from="threshold",values_from=c("previous_heatwave",
                                                   "hot_day","heatwave_day"))

#'Read in case-crossover dataset
casecross_list <- readRDS("../clean_data/casecross_list_HI.rds")

casecross_list <- casecross_list %>%
  map(~left_join(.,heatwave_days %>%
                   rename(Date=date) %>%
                   rename(county=COUNTY.NAME) %>%
                   mutate(Date=as.Date(Date)),
                 by=c("county","Date")))

####Heatwave Models####
#'Evaluate the effect of a prolonged increase in county-level heat index and risk 
#'for emergency department visits for certain cardiorespiratory and heat illness 
#'outcomes using two different definitions of a "heatwave".
outcomes <- c()
for (i in 1:length(casecross_list)) {
  outcomes <- c(outcomes,unique(casecross_list[[i]]$out_name))
}
print(outcomes)

#'Iterate through each outcome and each HI threshold to build conditional logistic
#'regression models with both definitions of heatwave as the exposure variable.
for(o in 1:length(outcomes)) {
  print(paste0("working on outcome ",o," of ",length(outcomes),": ",outcomes[o]))
  for(i in 1:length(thresholds)) {
    print(paste0("fitting models for threshold ",i," of ",length(thresholds),": ",thresholds[i]," degrees"))
    mod_hot_only <- NULL
    mod_prev_only <- NULL
    mod_red <- NULL
    mod_heat <- NULL
    
    df<-casecross_list[[o]]
    
    threshold <- thresholds[i]
    df_temp <- df %>%
      rename(previous_heatwave_relevant=!!paste0("previous_heatwave_",threshold)) %>% #prolonged heatwave
      rename(hot_day_relevant=!!paste0("hot_day_",threshold)) %>% #same-day elevated HI
      rename(heatwave_day_relevant=!!paste0("heatwave_day_",threshold)) %>% #acute heatwave
      select(contains("relevant"),outcome,ARITHMETIC.MEAN,hfdrepisode,Date)
    
    #'First build reduced conditional logistic regression model without an exposure of interest;
    #'only include same-day PM2.5 as a covariate.
    mod_red <- clogit(outcome ~ ARITHMETIC.MEAN + strata(hfdrepisode),
                      data=df_temp)
    
    #'Build model with just the "hot day" (binary variable for same-day above vs. below HI
    #'threshold) exposure and same-day PM2.5; define `good_hot` object to catch error
    #'if model cannot be fit d/t too few outcome on days above HI threshold.
    good_hot <- TRUE
    tryCatch(mod_hot_only <- clogit(outcome ~ hot_day_relevant + ARITHMETIC.MEAN + strata(hfdrepisode),
                                    data=df_temp), error = function(e) { good_hot <<- FALSE})
    
    #'If "hot day"-only model fit successfully, conduct LRT for hypothesis of statistical
    #'significance of same-day "hot day" in the model.
    if (good_hot) {
      LRT_hot_only <- anova(mod_red,mod_hot_only,test="LRT") %>%
        rename(p_hot_only="P(>|Chi|)")
      p_hot_only <- LRT_hot_only[2,"p_hot_only"]
      hot_only <- broom::tidy(mod_hot_only,conf.int=TRUE) %>% filter(term=="hot_day_relevant") %>%
        select(estimate,conf.low,conf.high) %>%
        rename(lower95=conf.low,upper95=conf.high) %>%
        setNames(paste0('hot_only.',names(.)))
    } else {
      p_hot_only <- NA
      hot_only <- tibble(
        hot_only.estimate = NA,
        hot_only.lower95 = NA,
        hot_only.upper95=NA
      )
    }
    
    #'Build model with both the same-day "hot day" and the prolonged heatwave (linear
    #'variable for number of previous consecutive days above HI threshold) exposures
    #'and same-day PM2.5. Also fit model with just prolonged heatwave exposure; define 
    #'`good_prev` and `good_hot_prev` objects to catch errors if models cannot be fit.
    good_prev <- TRUE
    good_hot_prev <- TRUE
    tryCatch(mod_prev_only <- clogit(outcome ~ previous_heatwave_relevant + ARITHMETIC.MEAN +
                                       strata(hfdrepisode), data=df_temp), 
             error = function(e) { good_prev <<- FALSE})
    tryCatch(mod_hot_prev <- clogit(outcome ~ previous_heatwave_relevant + hot_day_relevant +
                                     ARITHMETIC.MEAN + strata(hfdrepisode),data=df_temp),
             error = function(e) { good_hot_prev <<- FALSE})
    #'If prolonged heatwave-only model and prolonged heatwave plus same-day "hot day" 
    #'models fit successfully, conduct LRT for hypothesis of statistical significance of
    #'both same-day "hot day" and prolonged heatwave in the combined model.
    if (good_prev & good_hot_prev) {
      LRT_prev_minus_hot <- anova(mod_prev_only,mod_hot_prev, test="LRT") %>%
        rename(p_prev_minus_hot="P(>|Chi|)")
      p_prev_minus_hot <- LRT_prev_minus_hot[2,"p_prev_minus_hot"]
      LRT_hot_minus_prev <- anova(mod_hot_only,mod_hot_prev, test="LRT") %>%
        rename(p_hot_minus_prev="P(>|Chi|)")
      p_hot_minus_prev <- LRT_hot_minus_prev[2,"p_hot_minus_prev"]
      hot_prev_prev <- broom::tidy(mod_hot_prev,conf.int=TRUE) %>% filter(term == "previous_heatwave_relevant") %>%
        select(estimate, conf.low, conf.high) %>%
        rename(lower95 = conf.low, upper95 = conf.high) %>%
        setNames(paste0('hot_prev_prev.', names(.)))
      hot_prev_hot <- broom::tidy(mod_hot_prev,conf.int=TRUE) %>% filter(term=="hot_day_relevant") %>%
        select(estimate,conf.low, conf.high) %>%
        rename(lower95=conf.low,upper95=conf.high) %>% 
        setNames(paste0('hot_prev_hot.',names(.)))
    } else {
      p_prev_minus_hot <- NA
      p_hot_minus_prev <- NA
      hot_prev_prev <- tibble(
        hot_prev_prev.estimate = NA,
        hot_prev_prev.lower95 = NA,
        hot_prev_prev.upper95=NA
      )
      hot_prev_hot <- tibble(
        hot_prev_hot.estimate = NA,
        hot_prev_hot.lower95 = NA,
        hot_prev_hot.upper95=NA
      )
    }
    
    #'Build model with just the acute heatwave (binary variable for same-day and none
    #'preceding day above HI threshold) exposure and same-day PM2.5; define `good_heat`
    #'object to catch errors if model cannot be fit.
    good_heat <- TRUE
    tryCatch(mod_heat <- clogit(outcome ~ heatwave_day_relevant + ARITHMETIC.MEAN +
                                  strata(hfdrepisode),data=df_temp),
             error = function(e) { good_heat <<- FALSE})
    #'If acute heatwave-only model fits successfully, conduct LRT for hypothesis of 
    #'statistical significance of acute heatwave in the model.
    if (good_heat) {
      LRT_heat_only <- anova(mod_red,mod_heat,test="LRT") %>%
        rename(p_heat_only="P(>|Chi|)")
      p_heat_only <- LRT_heat_only[2,"p_heat_only"]
      heat_only <- broom::tidy(mod_heat,conf.int=TRUE) %>% filter(term=="heatwave_day_relevant") %>%
        select(estimate,conf.low,conf.high) %>%
        rename(lower95=conf.low,upper95=conf.high) %>%
        setNames(paste0('heat_only.',names(.)))
    } else {
      p_heat_only <- NA
      heat_only <- tibble(
        heat_only.estimate = NA,
        heat_only.lower95 = NA,
        heat_only.upper95=NA
      )
    }
    
    #'Compile model outputs for iterated outcome.
    est <- cbind(hot_only,
                 hot_prev_prev,
                 hot_prev_hot,
                 heat_only) %>%
      mutate(p_prev_minus_hot=p_prev_minus_hot,
             p_hot_minus_prev=p_hot_minus_prev,
             p_hot_only=p_hot_only,
             p_heat_only=p_heat_only)
    
    est <- est %>%
      mutate(threshold=threshold,
             outcome=unique(df$out_name))
    
    if (i==1) {
      outcome_heatwave <- est
    } else {
      outcome_heatwave <- bind_rows(outcome_heatwave, est)
    }
  }
  assign(paste0(outcomes[o],"_heatwave"),outcome_heatwave_mod)
}
#'Assemble all outcomes into list.
heatwave_mods_list <- list(
  asthma_heatwave,
  copd_heatwave,
  pneumonia_heatwave,bronchitis_heatwave,
  respiratory_heatwave,
  arrhythmia_heatwave,
  cerebrovascular_heatwave,
  ischemic_heatwave,
  myocardial_heatwave,
  heartfailure_heatwave,
  cardiovascular_heatwave,
  heatillness_heatwave,
  forearmfracture_heatwave,
  cardiorespiratory_heatwave
)

#Save all results as Excel spreadsheet.
file <- paste("../results/overall/heatwave/total_heatwave_analysis.xlsx")
wb <- createWorkbook()
sheets <- lapply(outcomes, createSheet, wb = wb)
void <- Map(addDataFrame, heatwave_mods_list, sheets)
saveWorkbook(wb, file = file)
