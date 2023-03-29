#This file is intended for analysis related to pre- and post- Covid trends

compare_pre_post_covid = function (df, dependent, regressor,
                                   pre_year, post_year) {
  
  df$dep = df[,dependent]
  df$reg = df[,regressor]
  
  pre_vals = df[df$år %in% c(pre_year),] %>%
    group_by(kommunenavn) %>%
    dplyr::summarise(mean_dep_pre = mean(dep, na.rm = TRUE),
              mean_reg_pre = mean(reg, na.rm = TRUE),
              total_count = n())
  
  post_vals = df[df$år %in% c(post_year),] %>%
    group_by(kommunenavn) %>%
    dplyr::summarise(mean_dep_post = mean(dep, na.rm = TRUE),
              mean_reg_post = mean(reg, na.rm = TRUE),
              median_income = mean(median_income, na.rm = TRUE),
              pop_dens = mean(pop_dens, na.rm = TRUE),
              total_count = n())
  
  #pre_vals = pre_vals[pre_vals$total_count > 280,]
  #post_vals = post_vals[post_vals$total_count > 280,]
  
  print(sprintf("Minimum single-county sample_size: %.0f",
        min(c(pre_vals$total_count, post_vals$total_count))))
  
  pre_vals = pre_vals[,!(names(pre_vals) %in% "total_count")]
  post_vals = post_vals[,!(names(post_vals) %in% "total_count")]
  
  grouped_vals = na.omit(full_join(pre_vals %>%
                              dplyr::mutate(id = row_number()), post_vals))
  print(sprintf("Number of counties: %.0f",
                nrow(grouped_vals)))

  grouped_vals$diff_dep = 
    grouped_vals$mean_dep_post - grouped_vals$mean_dep_pre
  
  reg_all_controls = lm(diff_dep~mean_reg_post + log(median_income) + 
             log(pop_dens), data = grouped_vals)
  reg_income = lm(diff_dep~mean_reg_post + log(median_income)
                  , data = grouped_vals)
  reg_pop_dens = lm(diff_dep~mean_reg_post + log(pop_dens)
                    , data = grouped_vals)
  reg_no_controls = lm(diff_dep~mean_reg_post, data = grouped_vals)
  
  return_var = 
    list("reg_all_controls" = summary(reg_all_controls),
         "reg_income" = summary(reg_income),
         "reg_pop_dens" = summary(reg_pop_dens),
         "reg_no_controls" = summary(reg_no_controls),
         "dataframe" = grouped_vals,
         "tex" = texreg(list(reg_all_controls,
                             reg_income,
                             reg_pop_dens,
                             reg_no_controls),
                             include.ci = FALSE))
  
  return(return_var)
}
#Example function call
#test_results = compare_pre_post_covid(trimmed_data, "depr_agg", "covid_prev")


