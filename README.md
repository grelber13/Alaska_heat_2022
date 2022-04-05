# Identifying temperature thresholds associated with increased risk of heat illness and cardiorespiratory impacts in Alaska
### Created by: Grace Kuiper
### Version: 4/4/2022

## Background

This repository provides R code that was used for analysis of Alaska weather and emergency department visit data in order to understand the impact of elevated heat index in Alaska on cardiorespiratory- and heat illness-related health outcomes. The finidngs of these analyses are available here: (Link to be updated after publication.)

## Methods

This was a case-crossover study to detect associations between elevated heat (as measured by heat index) and cardiorespiratory- and heat illness-related emergency department visits. The three largest population centers in Alaska were included in this study: Anchorage, Matanuska-Susitna Valley, and Fairbanks. Emergency department data were obtained from the Alaska Health Facilities Data Reporting Program for 2015-2019. Temperature and relative humidity data were obtained from the NOAA ASOS monitoring network, and each study site was assigned a daily heat index calculated from these two measurements during the summer months (June-August) for 2015-2019. Daily PM2.5 concentrations for each study site were included as a covariate in all models; these data were obtained from the Alaska Department of Environmental Conservation. 

For each emergency department visit that occurred within the study sites during the summer months of 2015-2019 that had a primary diagnosis indicative of a cardiorespiratory or heat illness event, patient age, sex, and race was available. The day on which the health event occurred was a case day, for which control days were sampled by selecting the same day of the week from every other week during the summer season of the same year.

### Threshold analysis

To identify potential temperature threshold for associated health risks in Alaska, temperature thresholds were set at 2-degree intervals from 70-90 deg. Fahrenheit. For each threshold, a binary variable was created to indicate if the heat index on a single day was above or below the threshold. Conditional logistic regression models were fit for each cardiorespiratory and heat illness outcome, with the heat index threhold indicator variable as the exposure. These models were repeated for all thresholds. Also, the study population was stratified by age group, sex, and race to identify potential differences by sociodemographic factors.

### Heatwave analysis

To understand the effect of prolonged elevated heat on cardiorespiratory and heat illness events among the Alaska population, two approaches were taken. The first evaluated the effect of acute heatwave, which was defined as a day on which the heat index exceeded the threshold and was immediately preceded by another day that exceeded the threshold. This definition of an acute heatwave was modeled using a binary variable, which was includedd in all conditional logistic regression models as the exposure of interest for each of the cardiorespiratory and heat illness outcomes across all thresholds.

The second approach was to evaluate the effect of an ongoing heatwave. For this approach, a linear variable was included in all models that was the number of previous consecutive days above the threshold. In this analysis, the indicator variable for same-day elevated heat index above the threshold was also included. As was done with the threshold analysis, all heatwave models were first fit using the entire dataset; then, stratified analyses were conducted by race, sex, and age group.

## Data

Available in the **/raw_data/** folder are the following files:
1. **daily_88101_0819_NCORE_Garden_Parkgate_Butte.xlsx** - this Excel spreadsheet contains daily PM2.5 data from the Alaska Department of Environmental Conservation.
2. **ipopdd2015v9_2019v1_ichs.txt** - this text file contains hospitalization data provided by the Alaska Health Facilities Data Reporting Program.
3. **hospital.txt** - this text file has metadata for hospitals across Alaska

## Scripts

Available in the **/scripts/** folder are the following files:
1. **00a_DEC_data_cleaning.R** - cleans PM2.5 covariate data
2. **00b_pull_NOAA_data.R** - pulls and cleans relative humidity and temperature data from the NOAA ASOS network using the `riem` and `rnoaa` packages
3. **00c_finalize_exposure_data.R** - merges and cleans PM2.5 and weather data to create dataset with single daily observations for each study site
4. **00d_HFDR_data_cleaning.R** - cleans HFDR emergency department visits data and creates case-crossover dataset
5. **00e_heat_data_cleaning.R** - calculates heat index and creates lagged heat index variables for the entire case-crossover dataset
6. **01_overall_threshold_analysis.R** - conducts the threshold analysis for the entire dataset using conditional logistic regression
7. **02_overall_heatwave_analysis.R** - conducts the acute and ongoing heatwave analyses for the entire dataset using conditional logistic regression
8. **03_stratified_threshold_analysis.R** - conducts stratifies threshold analyses using conditional logistic regression
9. **04_stratified_heatwave_analysis.R** - conducts stratified acute and ongoing ehatwave analyses using conditional logistic regression
