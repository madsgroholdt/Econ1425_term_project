#This file is intended to contain a class of general functions used
#for gaining summary information about the data

#Finds sample size of list of variables for which they are not NA
#INPUT: df - dataframe, vars - vector of vars to get sample size for
sample_size_finder = function (df, vars) {
  
  question_df = data.frame(question_name = vars)
  
  median_sample = c()
  unique_counties = c()
  total_sample = c()
  for (i in 1:length(vars)) {
    var_of_int = vars[i]

    trim_df = df[!is.na(df[,var_of_int]),]
    
    total_sample = nrow(trim_df)
    
    grouped_df = 
      trim_df %>% group_by(kommunenavn, år) %>%
      dplyr::summarise(total_count = dplyr::n())
    
    median_sample = append(median_sample,
                           median(grouped_df$total_count))
    unique_counties = append(unique_counties,
                             length(unique(grouped_df$kommunenavn)))
    
  }
  
  question_df$counties_represented = unique_counties
  question_df$median_sample_by_county_and_year = median_sample
  question_df$total_sample_size = total_sample
  
  return(question_df)
}
#Example function call:
#sample_size_finder(trimmed_data,
#                   c("livskval1", "livskval2", "livskval3", "livskval4"))


#Finds sample size for each kommune over time
#Args: dataframe to find sample sizes for each question
sample_size_kommune_year = function(df) {
  respondents_by_city_and_year = 
    df %>% group_by(kommunenavn, år) %>%
    dplyr::summarise(total_count = dplyr::n())
  
  return(respondents_by_city_and_year)
}
#Example function call
#sample_size_kommune_year(data_2022)

#Mean and median response rate finder
sample_size_averages = function(df) {
  
  mean_respondents = c()
  for (i in 1:ncol(df)) {
    sample = 
      nrow(df[!is.na(df[,i]),])
    
    mean_respondents = append(mean_respondents, sample)
    if (i %% 25 == 0) {
      print(sprintf("Done with row: %.0f", i))
    }
  }
  
  mean_question_response_rate = 
    mean(mean_respondents) / nrow(df)
  median_question_response_rate = 
    median(mean_respondents) / nrow(df)
  
  return(list(
    "mean_question_response_rate" = mean_question_response_rate,
    "median_question_response_rate" = median_question_response_rate
  ))
}
#Example function call
#sample_size_averages(data_2022)

#This graphs the relationship between two variables
graph_relationship = function (df, y_axis, x_axis,
                               title, label_x, label_y) {
  ggplot(df,
         aes(x = {{x_axis}},
             y = {{y_axis}})) +
    geom_point() + geom_smooth(method = "lm", se = FALSE) + 
    labs(x = label_x, y = label_y, title = title) +
    theme(plot.title = element_text(hjust = 0.5)) +
    #geom_text(aes(label= ifelse({{y_axis}} > quantile({{y_axis}}, 0.925) |
    #                              {{x_axis}} > quantile({{x_axis}}, 0.925) |
    #                            {{y_axis}} < quantile({{y_axis}}, 0.025) |
    #                              {{x_axis}} < quantile({{x_axis}}, 0.025),
    #                            as.character(kommunenavn),'')),
    #          hjust=0,vjust=0,size = 3.5)
    geom_text(label=df$kommunenavn, size = 3)
}
#Example function call
#graph_relationship(key_covid_results$depression_over_covid$dataframe,
#diff_dep, mean_reg_post,
#"Depression Against Covid Prevalence",
#"% Of Population Infected With Covid-19",
#"Change in Aggregate Depression Score")

#This scales its input variables to a standardized max value of 100
scale_vars = function (df, ignore_vars) {
  vars = colnames(df)
  for (i in 1:length(vars)) {
    df$var = df[,vars[i]]
    if (!(vars[i] %in% ignore_vars)) {
      df$var = 10 * ((df$var - min(df$var, na.rm = TRUE)) /
                        max(df$var, na.rm = TRUE))
    }
    df[,vars[i]] = df$var
    if (i %% 100 == 0) {
      print(sprintf("Done with row: %.0f", i))
    }
  }
  df = subset(df, select = -c(var))
  return(df)
}

#This negates values in the dataframe to account for non-logical
#survey response options
negate_vars = function (df, vars) {
  for (i in 1:length(vars)) {
    df$var = -df[,vars[i]]
    
    df[,vars[i]] = df$var
  }
  df = subset(df, select = -c(var))
  
  return(df)
}

