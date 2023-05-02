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

source(paste(path,
             "Sports Inclusion/Econ1425_term_project/data_files/initialize_environment.R",
             sep = ''))
#Clean unanswered questions and add additional variables
#FUNCTION LOCATIONS: initialize_environment.R
trimmed_data = add_new_vars_func(dataclean_func(data))

source(paste(path,
             "Sports Inclusion/Econ1425_term_project/data_files/general_functions.R",
             sep = ''))
#FUNCTION LOCATIONS: general_functions.R
#Scale all vars except Covid independent variables and categorical ones
trimmed_data = scale_vars(trimmed_data,
                          c("år", "skolenivå", "kommune", "fylke",
                            "kjønn", "klasse", "kommunenavn",
                            "befolkning", "covid_prev", "median_income",
                            "pop_dens", "unemployment_2021", "parent_hiding"))

#Negate relevant vars
trimmed_data = negate_vars(trimmed_data,
                   c("relig2", "orgmedl1", "friends_agg",
                     "family_trust", "total_trust", "spillprob1"))

#Get summary statistics for the surveys
#sample_size_summary = sample_size_averages(trimmed_data)
#Sample size by county and year
#county_and_year_sample_size = 
#  sample_size_kommune_year(trimmed_data)
#Key variable sample sizes
#key_var_sample_sizes = 
#  sample_size_finder(data_2022,
#                     colnames(data_2022))

source(paste(path,
             "Sports Inclusion/Econ1425_term_project/data_files/covid_analysis.R",
             sep = ''))

#Get regressions for change in dependent variable over covid
#Post-Covid year: 2021
#Pre-Covid year: 2020
#FUNCTION LOCATIONS: covid_analysis.R
#Extract info for single dependent variable as:
#key_covid_results$depression_over_covid$regression_all_controls
key_covid_results = list(
  "depression_over_covid" = 
    compare_pre_post_covid(trimmed_data,
                           "depr_agg", "covid_prev",
                           2020, 2021),
  "alc_and_drug_over_covid" = 
    compare_pre_post_covid(trimmed_data,
                           "alc_and_drug_agg", "covid_prev", 
                           2020, 2021),
  "lifequal_over_covid" = 
    compare_pre_post_covid(trimmed_data,
                           "lifequal_agg", "covid_prev", 
                           2020, 2021),
  "school_enjoyment_over_covid" = 
    compare_pre_post_covid(trimmed_data,
                           "school_enjoyment_agg", "covid_prev", 
                           2020, 2021),
  "training_over_covid" = 
    compare_pre_post_covid(trimmed_data,
                           "training_agg", "covid_prev", 
                           2020, 2021),
  "screentime_over_covid" = 
    compare_pre_post_covid(trimmed_data,
                           "skjermtid1", "covid_prev", 
                           2020, 2021),
  "friends_in_school_over_covid" = 
    compare_pre_post_covid(trimmed_data,
                           "friends_agg", "covid_prev", 
                           2020, 2021)
)

#This graphs interesting relationships
#FUNCTION LOCATION: general_functions.R
graph_relationship(key_covid_results$depression_over_covid$dataframe,
                   diff_dep, mean_reg_post,
                   "Sample-weighted Depression Against Covid Prevalence",
                   "% Of Population Infected With Covid-19",
                   "Change in Aggregate Depression Score")

graph_relationship(key_covid_results$alc_and_drug_over_covid$dataframe,
                   diff_dep, mean_reg_post,
                   "Sample-weighted Substance Use Against Covid Prevalence",
                   "% Of Population Infected With Covid-19",
                   "Change in Aggregate Substance Use Score")

graph_relationship(key_covid_results$lifequal_over_covid$dataframe,
                   -diff_dep, mean_reg_post,
                   "Sample-weighted life quality against Covid prevalence",
                   "% of population affected with Covid-19",
                   "Change in reported life quality")

graph_relationship(key_covid_results$school_enjoyment_over_covid$dataframe,
                   diff_dep, mean_reg_post,
                   "Sample-weighted school enjoyment against Covid prevalence",
                   "% of population affected with Covid-19",
                   "Change in reported school enjoyment")

graph_relationship(key_covid_results$training_over_covid$dataframe,
                   diff_dep, mean_reg_post,
                   "Sample-weighted physical activity against Covid prevalence",
                   "% of population affected with Covid-19",
                   "Change in physical activity")

graph_relationship(key_covid_results$screentime_over_covid$dataframe,
                   diff_dep, mean_reg_post,
                   "Sample-weighted screentime usage against Covid prevalence",
                   "% of population affected with Covid-19",
                   "Change in screentime usage")

graph_relationship(key_covid_results$friends_in_school_over_covid$dataframe,
                   -diff_dep, mean_reg_post,
                   "Sample-weighted friends against Covid prevalence",
                   "% Of Population Infected With Covid-19",
                   "Change in friends to spend time with during lunch break")

source(paste(path,
             "Sports Inclusion/Econ1425_term_project/data_files/non_covid_trends.R",
             sep = ''))

#Non-Covid relationships
key_covariate_results = list(
  "depression" = regression_with_controls(trimmed_data, "depr_agg"),
  "substance_use" = regression_with_controls(trimmed_data, "alc_and_drug_agg"),
  "school_enjoyment" = regression_with_controls(trimmed_data, "school_enjoyment_agg"),
  "fighting" = regression_with_controls(trimmed_data, "atfpro25"),
  "social_life" = regression_with_controls(trimmed_data, "friends_agg")
)

graph_binned_relationship(trimmed_data,
                          "training_agg", "depr_agg", 1,
                          "Aggregate Physical Activity",
                          "Aggregate Depression Indicator",
                          "Binned Physical Activity Against Depression")
graph_binned_relationship(trimmed_data,
                          "alc_and_drug_agg", "atfpro25", 1,
                          "Aggregate Substance Use",
                          "History of Violence",
                          "Binned Substance Use Against Violence")

graph_binned_relationship(trimmed_data,
                          "relig2", "probpers8", 0,
                          "", "", "")

#test with kinship indicators

scarce_regression_with_controls(trimmed_data, "family_trust", "total_trust")

