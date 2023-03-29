#This file is intended to contain functions needed to initialize the general
#analysis environment. After running this, any of the other files can be run
#on their own

dataclean_func = function(df) {

#Trim away all columns with <5% response rate
  trim_list = c()
  ultra_trim_list = c()
  cols = colnames(df)
  df_size = nrow(df)
  
  print("Starting removal of unanswered columns")
  
  for (i in 1:ncol(df)) {
    non_na_df = df[!is.na(df[,i]),c(3, i)]
    if (
      (nrow(non_na_df) / df_size) < 0.025
    ) {
      trim_list = append(trim_list, cols[i])
    }
    if (
      (nrow(non_na_df) / df_size) < 0.5
    ) {
      ultra_trim_list = append(ultra_trim_list, cols[i])
    }
    if(i %% 50 == 0) {
      print(cat("Done with column number: ", i))
    }
  }
  ultra_trim_list =
    ultra_trim_list[! ultra_trim_list %in% trim_list]
  trimmed_df = within(df, rm(list = trim_list))
  
  return(trimmed_df)
}  

add_new_vars_func = function(df) {
  
  #Aggregate variable for drug and alcohol usage
  df$alc_and_drug_agg = 
    df$alko1 + df$snus1 +
    df$røyk1 - df$hasj1
  
  #Aggregate variable for neighbourhood amenities 
  df$neigh_amenities = 
    df$nærtilb1 + df$nærtilb2 +
    df$nærtilb3 + df$nærtilb4
  
  #Aggregate variable for depression
  df$depr_agg = df$depr1 + df$depr2 +
    df$depr3 + df$depr4 + df$depr5 +
    df$depr6 + df$depr7
  
  #Aggregate variable for perceived life quality
  df$lifequal_agg = df$livskval1 + df$livskval2 + 
    df$livskval3 + df$livskval4 + df$livskval5 + 
    df$livskval6 + df$livskval7
  
  #Aggregate variable for school enjoyment 
  df$school_enjoyment_agg = 
    df$skole1 + df$skole2 + df$skole3 + 
    df$skole4 + df$skole6
  
  #Aggregate variable for physical activity
  df$training_agg =
    df$trener8 + df$trener9 + df$fysak60 * 1.2
  
  print("adding additional county and kommune names")
  
  #Add county names to df
  #county_names = read.csv("/Volumes/GoogleDrive/My Drive/Sophomore Spring/Econ 1425/Sports Inclusion/data/county_names_and_numbers.csv")
  
  #Add kommune names to df
  kommune_names = read.csv(
    paste(path,
          "Sports Inclusion/data/kommunenummer_og_navn.csv",
          sep = ''))
  kommune_names = kommune_names %>%
    dplyr::rename(kommunenavn = Beskrivelse)
  
  #Add in kommune names
  df = merge(df,
             kommune_names[,c("Kodeverdi", "kommunenavn")],
             by.x = "kommune", by.y = "Kodeverdi")
  
  print("Adding population density")
  
  #Adding population density by kommune
  kommune_popdens = read.csv(
    paste(path,
          "Sports Inclusion/data/popdens_by_kommune.csv",
          sep = ''))
  
  df = merge(df,
             kommune_popdens[,c("kommune", "pop_dens")],
             by.x = "kommune", by.y = "kommune", all.x = TRUE)
  
  print("Adding Covid Prevalence")
  
  #Adding Covid-data
  covid_cases = read.csv(
    paste(path,
          "Sports Inclusion/data/covid_count_by_county.csv",
          sep = ''))
  
  covid_cases = covid_cases %>%
    dplyr::rename(covid_cases_20 = X2020, covid_cases_21 = X2021)
  covid_cases$covid_cases_total = covid_cases$covid_cases_20 +
                                  covid_cases$covid_cases_21
  
  df = merge(df,
             covid_cases[,c("Kommune", "covid_cases_20",
                            "covid_cases_21", "covid_cases_total")],
             by.x = "kommunenavn", by.y = "Kommune", all.x = TRUE)
  
  print("Adding population and income numbers")
  
  #Adding population
  population_df = read.csv(
    paste(path,
          "Sports Inclusion/data/population_by_county.csv",
          sep = ''))
  
  df = merge(df,
             population_df[,c("befolkning", "kommunenummer")],
             by.x = "kommunenavn", by.y = "kommunenummer", all.x = TRUE)
  
  #Adding median income by kommune
  income_df = read.csv(
    paste(path,
          "Sports Inclusion/data/median_income_by_kommune.csv",
          sep = ''))
  
  df = merge(df,
             income_df[,c("median_income", "kommunenavn")],
             by.x = "kommunenavn", by.y = "kommunenavn", all.x = TRUE)
  
  #Adding covid prevalence variable as number of cases / population
  df$covid_prev =
    100 * (df$covid_cases_total / df$befolkning)
  
  return(df)
}

#END OF INITIALIZING ENVIRONMENT
