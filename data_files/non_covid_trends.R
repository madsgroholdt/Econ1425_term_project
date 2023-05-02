#This file contains functions related to regression analysis of
#general relationships in the dataset
regression_with_controls = function (df, dependent) {
  df$dep = df[,dependent]
  #Rescale dependent variable to get larger coefficients
  #for regressors
  df$dep = 10 * df$dep
  
  print(max(df$dep, na.rm = TRUE))
  print(min(df$dep, na.rm = TRUE))
  #Fixed effects regression
  reg_all_regressors = feols(dep ~ training_agg + media8 + relig2 + orgmedl1 | 
                             år + kommunenavn + round(søs, digits = 0) + 
                             klasse + kjønn + neigh_amenities, df,
                             cluster = "kommunenavn")
  
  reg_training = feols(dep ~ training_agg | 
                         år + kommunenavn + round(søs, digits = 0) + 
                         klasse + kjønn + neigh_amenities, df,
                       cluster = "kommunenavn")
  reg_gaming = feols(dep ~ media8 | 
                       år + kommunenavn + round(søs, digits = 0) + 
                       klasse + kjønn + neigh_amenities, df,
                     cluster = "kommunenavn")
  reg_religion = feols(dep ~ relig2 | 
                            år + kommunenavn + round(søs, digits = 0) + 
                            klasse + kjønn + neigh_amenities, df,
                       cluster = "kommunenavn")
  reg_ec_involvement  = feols(dep ~ orgmedl1 | 
                                år + kommunenavn + round(søs, digits = 0) + 
                                klasse + kjønn + neigh_amenities, df,
                              cluster = "kommunenavn")
  
  return_var = 
    list("reg_all_regressors" = summary(reg_all_regressors),
         "reg_training" = summary(reg_training),
         "reg_gaming" = summary(reg_gaming),
         "reg_religion" = summary(reg_religion),
         "reg_ec_involvement" = summary(reg_ec_involvement),
         "tex" = texreg(list(reg_all_regressors,
                             reg_training,
                             reg_gaming,
                             reg_religion,
                             reg_ec_involvement),
                        include.ci = FALSE))
  
  return(return_var)
}
#Example function call
#test_results = compare_pre_post_covid(trimmed_data, "depr_agg", "covid_prev")

#Attempt at graphing binned scatter plots
graph_binned_relationship = function(df, x_axis, y_axis, digs,
                                     label_x, label_y, title) {
  df$x_axis = df[,x_axis]
  df$y_axis = df[,y_axis]
  
  binned_df = df %>% 
    group_by(x_agg = round(x_axis * {{digs}}, digits = 0) / {{digs}}) %>%
    dplyr::summarise(mean_y = mean(y_axis, na.rm = TRUE))
  
  ggplot(binned_df, aes(x=x_agg, y=mean_y)) + 
    geom_point() + geom_smooth(method = "lm", se = FALSE) + 
    labs(x = label_x, y = label_y, title = title) +
    theme(plot.title = element_text(hjust = 0.5))
}

scarce_regression_with_controls = function (df, dependent, dependent2) {
  df$dep = df[,dependent]
  df$dep2 = df[,dependent2]
  #Rescale dependent variable to get larger coefficients
  #for regressors
  df$dep = 10 * df$dep
  df$dep2 = 10 * df$dep2
  
  print(max(df$dep, na.rm = TRUE))
  print(min(df$dep, na.rm = TRUE))
  print(max(df$dep2, na.rm = TRUE))
  print(min(df$dep2, na.rm = TRUE))
  #Fixed effects regression
  reg_all_regressors = feols(dep ~ media8 +
                               alc_and_drug_agg + fornøyd1 + depr_agg | 
                               år + kommunenavn + klasse + kjønn, df,
                             cluster = "kommunenavn")
  
  reg_gaming_problem = feols(dep ~ media8 + alc_and_drug_agg | 
                       år + kommunenavn + klasse + kjønn, df,
                     cluster = "kommunenavn")
  
  reg_depression = feols(dep ~ depr_agg | 
                           år + kommunenavn + klasse + kjønn, df,
                         cluster = "kommunenavn")
  
  reg_2 = feols(dep2 ~ media8 +
                  alc_and_drug_agg + fornøyd1 + depr_agg | 
                  år + kommunenavn + klasse + kjønn, df,
                cluster = "kommunenavn")
  
  reg_2_no_control = feols(dep2 ~ media8 + alc_and_drug_agg | 
                               år + kommunenavn + klasse + kjønn, df,
                             cluster = "kommunenavn")
  
  reg_2_depression = feols(dep2 ~ depr_agg | 
                             år + kommunenavn + klasse + kjønn, df,
                           cluster = "kommunenavn")
  
  return_var = 
    list("reg_all_regressors" = summary(reg_all_regressors),
         "reg_gaming_prob" = summary(reg_gaming_problem),
         "reg_depression" = summary(reg_depression),
         "reg_2" = summary(reg_2),
         "reg_2_no_control" = summary(reg_2_no_control),
         "reg_2_depression" = summary(reg_2_depression),
         "tex" = texreg(list(reg_all_regressors,
                             reg_gaming_problem,
                             reg_depression,
                             reg_2,
                             reg_2_no_control,
                             reg_2_depression),
                        include.ci = FALSE,
                        custom.coef.names=c("Gaming",
                                            "Substance Use",
                                            "Satisfaction With Parents",
                                            "Depression"),
                        custom.header = list("Confiding in Parents" = 1:3,
                                             "Confiding in Others" = 4:6)))
  
  return(return_var)
}
