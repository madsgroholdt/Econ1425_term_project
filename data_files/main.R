#This file contains all main function calls used for analysis
#Load functions from the various files commented below, then 
#run this file to get the various results
library(haven)
library(fixest)
library(corrplot)
library(tidyverse)
library(plyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(lfe)
library(estimatr)
library(magrittr)
library(texreg)

path = "/Volumes/GoogleDrive/My Drive/Sophomore Spring/Econ 1425/"

#Load main survey datasets
data = 
  read_sav(
    paste(path, "Sports Inclusion/data/NSD3007.sav", sep = '')
  )
data_2022 = 
  read_sav(
    paste(path,
    "Sports Inclusion/data/ungdata_2/ungdata_2010_to_2022",
    sep = '')
  )

source(paste(path,
             "Sports Inclusion/data_files/initialize_environment.R",
             sep = ''))
#Clean unanswered questions and add additional variables
#FUNCTION LOCATIONS: initialize_environment.R
trimmed_data = add_new_vars_func(dataclean_func(data))
data_2022 = add_new_vars_func(data_2022)

source(paste(path,
             "Sports Inclusion/data_files/general_functions.R",
             sep = ''))
#Get summary statistics for the surveys
#FUNCTION LOCATIONS: general_functions.R
sample_size_summary = sample_size_averages(data_2022)
#Sample size by county and year
county_and_year_sample_size = 
  sample_size_kommune_year(data_2022)
#Key variable sample sizes
key_var_sample_sizes = 
  sample_size_finder(data_2022,
                     colnames(data_2022))

source(paste(path,
             "Sports Inclusion/data_files/covid_analysis.R",
             sep = ''))

#Get regressions for change in dependent variable over covid
#Post-Covid year: 2021
#Pre-Covid year: 2020
#FUNCTION LOCATIONS: covid_analysis.R
#Extract info for single dependent variable as:
#key_covid_results$depression_over_covid$regression_all_controls
key_covid_results = list(
  "depression_over_covid" = 
    compare_pre_post_covid(trimmed_data, "depr_agg", "covid_prev",
                           2020, 2021),
  "alc_and_drug_over_covid" = 
    compare_pre_post_covid(trimmed_data, "alc_and_drug_agg", "covid_prev", 
                           2020, 2021),
  "lifequal_over_covid" = 
    compare_pre_post_covid(trimmed_data, "lifequal_agg", "covid_prev", 
                           2020, 2021),
  "school_enjoyment_over_covid" = 
    compare_pre_post_covid(trimmed_data, "school_enjoyment_agg", "covid_prev", 
                           2020, 2021),
  "training_over_covid" = 
    compare_pre_post_covid(trimmed_data, "training_agg", "covid_prev", 
                           2020, 2021),
  "screentime_over_covid" = 
    compare_pre_post_covid(trimmed_data, "skjermtid1", "covid_prev", 
                           2020, 2021),
  "friends_in_school_over_covid" = 
    compare_pre_post_covid(trimmed_data, "vennskole", "covid_prev", 
                           2020, 2021)
)

#Get regressions for change in dependent variable over covid
#Post-Covid year: 2021
#Pre-Covid year: 2020
#FUNCTION LOCATIONS: covid_analysis.R
key_covid_results_2022_data = list(
  "depression_over_covid" = 
    compare_pre_post_covid(data_2022, "depr_agg", "covid_prev",
                           2020, 2021),
  "alc_and_drug_over_covid" = 
    compare_pre_post_covid(data_2022, "alc_and_drug_agg", "covid_prev",
                           2020, 2021),
  "lifequal_over_covid" = 
    compare_pre_post_covid(data_2022, "lifequal_agg", "covid_prev",
                           2020, 2021),
  "school_enjoyment_over_covid" = 
    compare_pre_post_covid(data_2022, "school_enjoyment_agg", "covid_prev",
                           2020, 2021),
  "training_over_covid" = 
    compare_pre_post_covid(data_2022, "training_agg", "covid_prev",
                           2020, 2021),
  "screentime_over_covid" = 
    compare_pre_post_covid(data_2022, "skjermtid1", "covid_prev",
                           2020, 2021),
  "friends_in_school_over_covid" = 
    compare_pre_post_covid(data_2022, "vennskole", "covid_prev",
                           2020, 2021)
)


#This graphs interesting relationships
#FUNCTION LOCATION: general_functions.R
graph_relationship(key_covid_results$depression_over_covid$dataframe,
                   diff_dep, mean_reg_post,
                   "Depression Against Covid Prevalence",
                   "% Of Population Infected With Covid-19",
                   "Change in Aggregate Depression Score")

graph_relationship(key_covid_results$friends_in_school_over_covid$dataframe,
                   -diff_dep, mean_reg_post,
                   "Friends in school against Covid prevalence",
                   "% Of Population Infected With Covid-19",
                   "Change in friends to spend time with during lunch break")

graph_relationship(key_covid_results$lifequal_over_covid$dataframe,
                   -diff_dep, mean_reg_post,
                   "Reported life quality against Covid prevalence",
                   "% of population affected with Covid-19",
                   "Change in reported life quality")

graph_relationship(key_covid_results_2022_data$friends_in_school_over_covid$dataframe,
                   -diff_dep, mean_reg_post,
                   "Friends in school against Covid prevalence",
                   "% Of Population Infected With Covid-19",
                   "Change in friends to spend time with during lunch break")


#RANDOM CODE WRITTEN ON DIFFERENT PAGES

ultra_trimmed_data = within(trimmed_data, rm(list = ultra_trim_list))

missing_counties = 
  unique(trimmed_data[is.na(c(
    trimmed_data$befolkning, trimmed_data$covid_prev, trimmed_data$median_income
  )),]$kommunenavn)

missing_counties[
  which(missing_counties %in% 
          unique(trimmed_data[trimmed_data$år == 2021,]$kommunenavn))]

#Imports cool relationships-df from cool_relationships_finder.R
cool_relationships = read.csv(
  paste(path,
        "Sports Inclusion/data_files/output/cool_regression_relationships.csv",
        sep = ''))





