## ---------------------------
## Script name: Heat index threshold analysis among stratified study population
## Author: Grace Kuiper
## Version: 08/18/2021

## Purpose: For the purpose of AK heat analysis using heat index based on maximum daily 
##          temperature:identify threshold values for cardiovascular and heat illness 
##          outcomes using conditional logistic regression models for a case-crossover 
##          study design among study population stratified by age, sex, and race.

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

#'Read in case-crossover dataset
casecross_list <- readRDS("../clean_data/casecross_list_HI.rds")

#'For each strata and group, the following objects were changed before the script was
#'run: `strata` and `group`. For `strata <- "sex"`, `group` can be "F" or "M".
#'For `strata <- "race"`, `group` can be "Alaskan Native" or "Non-Alaskan Native".
#'For `strata <- "age"`, `geroup` can be "<15", "15-65", or ">65".
strata <- "sex"
group <- "F"

#'Filter case-crossover dataset to only include strata group of interest.
casecross_list <- casecross_list %>%
  map(~mutate(.,race=ifelse(race==3,"Alaskan Native",
                            ifelse(race<9 & race!=3,"Non-Alaskan Native",NA))))

casecross_list <- casecross_list %>%
  map(~mutate(.,age_cat=ifelse(age<15,"<15",
                               ifelse(age<66,"15-65",
                                     ">65")))) %>%
  map(~select(.,-age)) %>%
  map(~rename(.,age=age_cat))
casecross_list <- casecross_list %>%
  map(~rename(.,strata=!!strata)) %>%
  map(~filter(.,strata==group))

####Same-Day Associations####
#'Evaluate an increase in county-level same-day daily heat index and risk for emergency 
#'department visits for certain cardiorespiratory and heat illness outcomes.
outcomes <- c()
for (i in 1:length(casecross_list)) {
  outcomes <- c(outcomes,unique(casecross_list[[i]]$out_name))
}
print(outcomes)

thresholds <- c(70,72,74,76,78,80,82,84,86,88,90)

#'Iterate through each threshold to build conditional logistic regression models.
for (i in 1:length(thresholds)) {
  no_outcomes <- c()
  #'First remove outcomes from the case-crossover dataset if there are no ED visits
  #'on days above the HI threshold.
  for (j in 1:length(casecross_list)) {
    if (nrow(casecross_list[[j]] %>% filter(HI>thresholds[i]) %>% filter(outcome==1))==0) {
      no_outcomes <- c(no_outcomes,j)
    }
  }
  if (length(no_outcomes)>0) {
    casecross_list_temp <- casecross_list[c(-no_outcomes)]
  } else {casecross_list_temp <- casecross_list}
  threshold_temp <- thresholds[i]
  
  #'Define outcome variable as binary above vs. below the daily HI threshold
  casecross_list_temp <- casecross_list_temp %>%
    map(~mutate(.,threshold=thresholds[i])) %>%
    map(~mutate(.,aboveYN=ifelse(HI>threshold,1,0)))

  temp_outcomes <- c()
  for (k in 1:length(casecross_list_temp)) {
    temp_outcomes <- c(temp_outcomes,unique(casecross_list_temp[[k]]$out_name))
  }

  #'For each outcome (separate dataframe in case-crossover dataset list object),
  #'build conditional logistic regression model
  HI_results_threshold <- casecross_list_temp %>%
    map_dfr(. , function(df){
      #'First build a model with the exposure of interest (above vs. below daily
      #'HI threshold) and PM2.5 as covariate, with matching by patient, per the case-
      #'crossover study design of selecting control days for each patient's case
      #'day of ED visit.
      mod_ind <- clogit(outcome ~ aboveYN +
                      ARITHMETIC.MEAN + strata(hfdrepisode), data = df)
      #'Also build reduced model without the exposure of interest.
      mod_red <- clogit(outcome ~ ARITHMETIC.MEAN + strata(hfdrepisode),
                        data=df)
      #'Use LRT for hypothesis testing of significance of exposure in model.
      LRT_out_red <- anova(mod_red,mod_ind, test="LRT") %>%
        rename(p_red=`P(>|Chi|)`)
      p_red <- LRT_out_red[2,"p_red"]

      est <- cbind(HI_est <- broom::tidy(mod_ind,conf.int=TRUE) %>% filter(term == "aboveYN") %>%
                     select(estimate, conf.low, conf.high) %>%
                     rename(lower95 = conf.low, upper95 = conf.high) %>%
                     setNames(paste0('HI_threshold.', names(.))) %>%
                     mutate_all(exp),
                   PM_est <- broom::tidy(mod_ind,conf.int=TRUE) %>% filter(term == "ARITHMETIC.MEAN") %>%
                     select(estimate, conf.low, conf.high) %>%
                     rename(lower95 = conf.low, upper95 = conf.high) %>%
                     setNames(paste0('PM.', names(.))) %>%
                     mutate_all(exp)) %>%
        mutate(p_red=p_red)
    }) %>% 
    cbind(temp_outcomes, .) %>%
    rename(outcomes=temp_outcomes)

  #'Save all model outputs for single threshold as dataframe
  assign(paste0("HI_results_threshold",thresholds[i]),HI_results_threshold)
  #'Also compile model outputs for each threshold into total results dataframe
  if (i==1) {
    HI_results_threshold_total <- HI_results_threshold %>%
      mutate(threshold=thresholds[i])
  } else {
    HI_results_threshold_total <- bind_rows(HI_results_threshold_total,
                                            HI_results_threshold %>%
                                              mutate(threshold=thresholds[i]))
  }
}

#'Print and save results
print(HI_results_threshold_total)
saveRDS(HI_results_threshold_total,paste0("../results/stratified/threshold/",
                                          strata,"/",group,"/HI_results_threshold_total.rds"))

####Lag Day 2####
#'Repeat the above methods using exposure of interest as above vs. below HI threshold
#'on Lag Day 2; also include lagged PM2.5 as covariate in models.
for (i in 1:length(thresholds)) {
  no_outcomes <- c()
  for (j in 1:length(casecross_list)) {
    if (nrow(casecross_list[[j]] %>% filter(HI_lag1>thresholds[i]) %>% filter(outcome==1))==0) {
      no_outcomes <- c(no_outcomes,j)
    }
  }
  if (length(no_outcomes)>0) {
    casecross_list_temp <- casecross_list[c(-no_outcomes)]
  } else {casecross_list_temp <- casecross_list}
  threshold_temp <- thresholds[i]
  casecross_list_temp <- casecross_list_temp %>%
    map(~mutate(.,threshold=thresholds[i])) %>%
    map(~mutate(.,aboveYN=ifelse(HI_lag1>threshold,1,0)))

  temp_outcomes <- c()
  for (k in 1:length(casecross_list_temp)) {
    temp_outcomes <- c(temp_outcomes,unique(casecross_list_temp[[k]]$out_name))
  }

  HI_lag1_results_threshold <- casecross_list_temp %>%
    map_dfr(. , function(df){
      mod_ind <- clogit(outcome ~ aboveYN +
                          ARITHMETIC.MEAN_lag1 + strata(hfdrepisode), data = df)
      mod_red <- clogit(outcome ~ ARITHMETIC.MEAN_lag1 + strata(hfdrepisode),
                        data=df)
      LRT_out_red <- anova(mod_red,mod_ind, test="LRT") %>%
        rename(p_red=`P(>|Chi|)`)
      p_red <- LRT_out_red[2,"p_red"]

      est <- cbind(HI_est <- broom::tidy(mod_ind,conf.int=TRUE) %>% filter(term == "aboveYN") %>%
                     select(estimate, conf.low, conf.high) %>%
                     rename(lower95 = conf.low, upper95 = conf.high) %>%
                     setNames(paste0('HI_threshold.', names(.))) %>%
                     mutate_all(exp),
                   PM_est <- broom::tidy(mod_ind,conf.int=TRUE) %>% filter(term == "ARITHMETIC.MEAN_lag1") %>%
                     select(estimate, conf.low, conf.high) %>%
                     rename(lower95 = conf.low, upper95 = conf.high) %>%
                     setNames(paste0('PM.', names(.))) %>%
                     mutate_all(exp)) %>%
        mutate(p_red=p_red)
    }) %>% # end map
    cbind(temp_outcomes, .) %>%
    rename(outcomes=temp_outcomes)

  assign(paste0("HI_lag1_results_threshold",thresholds[i]),HI_lag1_results_threshold)
  if (i==1) {
    HI_lag1_results_threshold_total <- HI_lag1_results_threshold %>%
      mutate(threshold=thresholds[i])
  } else {
    HI_lag1_results_threshold_total <- bind_rows(HI_lag1_results_threshold_total,
                                            HI_lag1_results_threshold %>%
                                              mutate(threshold=thresholds[i]))
  }
}

#'Print and save results
print(HI_lag1_results_threshold_total)
saveRDS(HI_lag1_results_threshold_total,paste0("../results/stratified/threshold/",
                                               strata,"/",group,"/HI_lag1_results_threshold_total.rds"))

####Lag Day 2####
#'Repeat the above methods using exposure of interest as above vs. below HI threshold
#'on Lag Day 2; also include lagged PM2.5 as covariate in models.
for (i in 1:(length(thresholds))) {
  no_outcomes <- c()
  for (j in 1:length(casecross_list)) {
    if (nrow(casecross_list[[j]] %>% filter(HI_lag2>thresholds[i]) %>% filter(outcome==1))==0) {
      no_outcomes <- c(no_outcomes,j)
    }
  }
  if (length(no_outcomes)>0) {
    casecross_list_temp <- casecross_list[c(-no_outcomes)]
  } else {casecross_list_temp <- casecross_list}
  threshold_temp <- thresholds[i]
  casecross_list_temp <- casecross_list_temp %>%
    map(~mutate(.,threshold=thresholds[i])) %>%
    map(~mutate(.,aboveYN=ifelse(HI_lag2>threshold,1,0)))

  temp_outcomes <- c()
  for (k in 1:length(casecross_list_temp)) {
    temp_outcomes <- c(temp_outcomes,unique(casecross_list_temp[[k]]$out_name))
  }

  HI_lag2_results_threshold <- casecross_list_temp %>%
    map_dfr(. , function(df){
      mod_ind <- clogit(outcome ~ aboveYN +
                          ARITHMETIC.MEAN_lag2 + strata(hfdrepisode), data = df)
      mod_red <- clogit(outcome ~ ARITHMETIC.MEAN_lag2 + strata(hfdrepisode),
                        data=df)
      LRT_out_red <- anova(mod_red,mod_ind, test="LRT") %>%
        rename(p_red=`P(>|Chi|)`)
      p_red <- LRT_out_red[2,"p_red"]

      est <- cbind(HI_est <- broom::tidy(mod_ind,conf.int=TRUE) %>% filter(term == "aboveYN") %>%
                     select(estimate, conf.low, conf.high) %>%
                     rename(lower95 = conf.low, upper95 = conf.high) %>%
                     setNames(paste0('HI_threshold.', names(.))) %>%
                     mutate_all(exp),
                   PM_est <- broom::tidy(mod_ind,conf.int=TRUE) %>% filter(term == "ARITHMETIC.MEAN_lag2") %>%
                     select(estimate, conf.low, conf.high) %>%
                     rename(lower95 = conf.low, upper95 = conf.high) %>%
                     setNames(paste0('PM.', names(.))) %>%
                     mutate_all(exp)) %>%
        mutate(p_red=p_red)
    }) %>% # end map
    cbind(temp_outcomes, .) %>%
    rename(outcomes=temp_outcomes)

  assign(paste0("HI_lag2_results_threshold",thresholds[i]),HI_lag2_results_threshold)
  if (i==1) {
    HI_lag2_results_threshold_total <- HI_lag2_results_threshold %>%
      mutate(threshold=thresholds[i])
  } else {
    HI_lag2_results_threshold_total <- bind_rows(HI_lag2_results_threshold_total,
                                                 HI_lag2_results_threshold %>%
                                                   mutate(threshold=thresholds[i]))
  }
}

#'Print and save results
print(HI_lag2_results_threshold_total)
saveRDS(HI_lag2_results_threshold_total,paste0("../results/stratified/threshold/",
                                               strata,"/",group,"/HI_lag2_results_threshold_total.rds"))

####Lag Day 3####
#'Repeat the above methods using exposure of interest as above vs. below HI threshold
#'on Lag Day 3; also include lagged PM2.5 as covariate in models.
for (i in 1:(length(thresholds))) {
  no_outcomes <- c()
  for (j in 1:length(casecross_list)) {
    if (nrow(casecross_list[[j]] %>% filter(HI_lag3>thresholds[i]) %>% filter(outcome==1))==0) {
      no_outcomes <- c(no_outcomes,j)
    }
  }
  if (length(no_outcomes)>0) {
    casecross_list_temp <- casecross_list[c(-no_outcomes)]
  } else {casecross_list_temp <- casecross_list}
  threshold_temp <- thresholds[i]
  casecross_list_temp <- casecross_list_temp %>%
    map(~mutate(.,threshold=thresholds[i])) %>%
    map(~mutate(.,aboveYN=ifelse(HI_lag3>threshold,1,0)))

  temp_outcomes <- c()
  for (k in 1:length(casecross_list_temp)) {
    temp_outcomes <- c(temp_outcomes,unique(casecross_list_temp[[k]]$out_name))
  }

  HI_lag3_results_threshold <- casecross_list_temp %>%
    map_dfr(. , function(df){
      mod_ind <- clogit(outcome ~ aboveYN +
                          ARITHMETIC.MEAN_lag3 + strata(hfdrepisode), data = df)
      mod_red <- clogit(outcome ~ ARITHMETIC.MEAN_lag3 + strata(hfdrepisode),
                        data=df)
      LRT_out_red <- anova(mod_red,mod_ind, test="LRT") %>%
        rename(p_red=`P(>|Chi|)`)
      p_red <- LRT_out_red[2,"p_red"]

      est <- cbind(HI_est <- broom::tidy(mod_ind,conf.int=TRUE) %>% filter(term == "aboveYN") %>%
                     select(estimate, conf.low, conf.high) %>%
                     rename(lower95 = conf.low, upper95 = conf.high) %>%
                     setNames(paste0('HI_threshold.', names(.))) %>%
                     mutate_all(exp),
                   PM_est <- broom::tidy(mod_ind,conf.int=TRUE) %>% filter(term == "ARITHMETIC.MEAN_lag3") %>%
                     select(estimate, conf.low, conf.high) %>%
                     rename(lower95 = conf.low, upper95 = conf.high) %>%
                     setNames(paste0('PM.', names(.))) %>%
                     mutate_all(exp)) %>%
        mutate(p_red=p_red)
    }) %>% # end map
    cbind(temp_outcomes, .) %>%
    rename(outcomes=temp_outcomes)

  assign(paste0("HI_lag3_results_threshold",thresholds[i]),HI_lag3_results_threshold)
  if (i==1) {
    HI_lag3_results_threshold_total <- HI_lag3_results_threshold %>%
      mutate(threshold=thresholds[i])
  } else {
    HI_lag3_results_threshold_total <- bind_rows(HI_lag3_results_threshold_total,
                                                 HI_lag3_results_threshold %>%
                                                   mutate(threshold=thresholds[i]))
  }
}

#'Print and save results
print(HI_lag3_results_threshold_total)
saveRDS(HI_lag3_results_threshold_total,paste0("../results/stratified/threshold/",
                                               strata,"/",group,"/HI_lag3_results_threshold_total.rds"))

####Lag Day 4####
#'Repeat the above methods using exposure of interest as above vs. below HI threshold
#'on Lag Day 4; also include lagged PM2.5 as covariate in models.
for (i in 1:length(thresholds)) {
  no_outcomes <- c()
  for (j in 1:length(casecross_list)) {
    if (nrow(casecross_list[[j]] %>% filter(HI_lag4>thresholds[i]) %>% filter(outcome==1))==0) {
      no_outcomes <- c(no_outcomes,j)
    }
  }
  if (length(no_outcomes)>0) {
    casecross_list_temp <- casecross_list[c(-no_outcomes)]
  } else {casecross_list_temp <- casecross_list}
  threshold_temp <- thresholds[i]
  casecross_list_temp <- casecross_list_temp %>%
    map(~mutate(.,threshold=thresholds[i])) %>%
    map(~mutate(.,aboveYN=ifelse(HI_lag4>threshold,1,0)))

  temp_outcomes <- c()
  for (k in 1:length(casecross_list_temp)) {
    temp_outcomes <- c(temp_outcomes,unique(casecross_list_temp[[k]]$out_name))
  }

  HI_lag4_results_threshold <- casecross_list_temp %>%
    map_dfr(. , function(df){
      mod_ind <- clogit(outcome ~ aboveYN +
                          ARITHMETIC.MEAN_lag4 + strata(hfdrepisode), data = df)
      mod_red <- clogit(outcome ~ ARITHMETIC.MEAN_lag4 + strata(hfdrepisode),
                        data=df)
      LRT_out_red <- anova(mod_red,mod_ind, test="LRT") %>%
        rename(p_red=`P(>|Chi|)`)
      p_red <- LRT_out_red[2,"p_red"]

      est <- cbind(HI_est <- broom::tidy(mod_ind,conf.int=TRUE) %>% filter(term == "aboveYN") %>%
                     select(estimate, conf.low, conf.high) %>%
                     rename(lower95 = conf.low, upper95 = conf.high) %>%
                     setNames(paste0('HI_threshold.', names(.))) %>%
                     mutate_all(exp),
                   PM_est <- broom::tidy(mod_ind,conf.int=TRUE) %>% filter(term == "ARITHMETIC.MEAN_lag4") %>%
                     select(estimate, conf.low, conf.high) %>%
                     rename(lower95 = conf.low, upper95 = conf.high) %>%
                     setNames(paste0('PM.', names(.))) %>%
                     mutate_all(exp)) %>%
        mutate(p_red=p_red)
    }) %>% # end map
    cbind(temp_outcomes, .) %>%
    rename(outcomes=temp_outcomes)

  assign(paste0("HI_lag4_results_threshold",thresholds[i]),HI_lag4_results_threshold)
  if (i==1) {
    HI_lag4_results_threshold_total <- HI_lag4_results_threshold %>%
      mutate(threshold=thresholds[i])
  } else {
    HI_lag4_results_threshold_total <- bind_rows(HI_lag4_results_threshold_total,
                                                 HI_lag4_results_threshold %>%
                                                   mutate(threshold=thresholds[i]))
  }
}

#'Print and save results
print(HI_lag4_results_threshold_total)
saveRDS(HI_lag4_results_threshold_total,paste0("../results/stratified/threshold/",
                                               strata,"/",group,"/HI_lag4_results_threshold_total.rds"))

####Lag Day 5####
#'Repeat the above methods using exposure of interest as above vs. below HI threshold
#'on Lag Day 5; also include lagged PM2.5 as covariate in models.
for (i in 1:length(thresholds)) {
  no_outcomes <- c()
  for (j in 1:length(casecross_list)) {
    if (nrow(casecross_list[[j]] %>% filter(HI_lag5>thresholds[i]) %>% filter(outcome==1))==0) {
      no_outcomes <- c(no_outcomes,j)
    }
  }
  if (length(no_outcomes)>0) {
    casecross_list_temp <- casecross_list[c(-no_outcomes)]
  } else {casecross_list_temp <- casecross_list}
  threshold_temp <- thresholds[i]
  casecross_list_temp <- casecross_list_temp %>%
    map(~mutate(.,threshold=thresholds[i])) %>%
    map(~mutate(.,aboveYN=ifelse(HI_lag5>threshold,1,0)))

  temp_outcomes <- c()
  for (k in 1:length(casecross_list_temp)) {
    temp_outcomes <- c(temp_outcomes,unique(casecross_list_temp[[k]]$out_name))
  }

  HI_lag5_results_threshold <- casecross_list_temp %>%
    map_dfr(. , function(df){
      mod_ind <- clogit(outcome ~ aboveYN +
                          ARITHMETIC.MEAN_lag5 + strata(hfdrepisode), data = df)
      mod_red <- clogit(outcome ~ ARITHMETIC.MEAN_lag5 + strata(hfdrepisode),
                        data=df)
      LRT_out_red <- anova(mod_red,mod_ind, test="LRT") %>%
        rename(p_red=`P(>|Chi|)`)
      p_red <- LRT_out_red[2,"p_red"]

      est <- cbind(HI_est <- broom::tidy(mod_ind,conf.int=TRUE) %>% filter(term == "aboveYN") %>%
                     select(estimate, conf.low, conf.high) %>%
                     rename(lower95 = conf.low, upper95 = conf.high) %>%
                     setNames(paste0('HI_threshold.', names(.))) %>%
                     mutate_all(exp),
                   PM_est <- broom::tidy(mod_ind,conf.int=TRUE) %>% filter(term == "ARITHMETIC.MEAN_lag5") %>%
                     select(estimate, conf.low, conf.high) %>%
                     rename(lower95 = conf.low, upper95 = conf.high) %>%
                     setNames(paste0('PM.', names(.))) %>%
                     mutate_all(exp)) %>%
        mutate(p_red=p_red)
    }) %>% # end map
    cbind(temp_outcomes, .) %>%
    rename(outcomes=temp_outcomes)

  assign(paste0("HI_lag5_results_threshold",thresholds[i]),HI_lag5_results_threshold)
  if (i==1) {
    HI_lag5_results_threshold_total <- HI_lag5_results_threshold %>%
      mutate(threshold=thresholds[i])
  } else {
    HI_lag5_results_threshold_total <- bind_rows(HI_lag5_results_threshold_total,
                                                 HI_lag5_results_threshold %>%
                                                   mutate(threshold=thresholds[i]))
  }
}

#'Print and save results
print(HI_lag5_results_threshold_total)
saveRDS(HI_lag5_results_threshold_total,paste0("../results/stratified/threshold/",
                                               strata,"/",group,"/HI_lag5_results_threshold_total.rds"))