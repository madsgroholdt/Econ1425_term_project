#This file is intended for analysis related to pre- and post- Covid trends

compare_pre_post_covid = function (df, dependent, regressor,
                                   pre_year, post_year) {
  
  df$dep = df[,dependent]
  df$reg = df[,regressor]
  
  pre_vals = df[df$år %in% c(pre_year),] %>%
    group_by(kommunenavn) %>%
    dplyr::summarise(mean_dep_pre = mean(dep, na.rm = TRUE),
              mean_reg_pre = mean(reg, na.rm = TRUE),
              unemployment_pre = mean(unemployment_2021, na.rm = TRUE),
              total_count = n())
  
  post_vals = df[df$år %in% c(post_year),] %>%
    group_by(kommunenavn) %>%
    dplyr::summarise(mean_dep_post = mean(dep, na.rm = TRUE),
              mean_reg_post = mean(reg, na.rm = TRUE),
              median_income = mean(median_income, na.rm = TRUE),
              pop_dens = mean(pop_dens, na.rm = TRUE),
              pop = mean(befolkning, na.rm = TRUE),
              unemployment_post = mean(unemployment_2021, na.rm = TRUE),
              total_count = n())
  
  print(sprintf("Minimum single-county sample_size: %.0f",
        min(c(pre_vals$total_count, post_vals$total_count))))
  print(sprintf("Maximum single-county sample_size: %.0f",
                max(c(pre_vals$total_count, post_vals$total_count))))
  
  pre_vals = pre_vals[,!(names(pre_vals) %in% "total_count")]
  #post_vals = post_vals[,!(names(post_vals) %in% "total_count")]
  
  grouped_vals = na.omit(full_join(pre_vals %>%
                              dplyr::mutate(id = row_number()), post_vals))
  print(sprintf("Number of counties: %.0f",
                nrow(grouped_vals)))
  tot_sample_size = nrow(trimmed_data[
    trimmed_data$kommunenavn %in% unique(grouped_vals$kommunenavn),])
  print(sprintf("Total Sample Size: %.0f",
                tot_sample_size))

  grouped_vals$diff_dep = 
    10 * (grouped_vals$mean_dep_post - grouped_vals$mean_dep_pre) * log(100 *
    grouped_vals$total_count / max(grouped_vals$total_count))
  
  grouped_vals$diff_unemployment = 
    grouped_vals$unemployment_post - grouped_vals$unemployment_pre
  
  reg_all_controls = lm(diff_dep~mean_reg_post + log(median_income) + 
                        log(pop_dens) + diff_unemployment, data = grouped_vals)
  reg_income = lm(diff_dep~mean_reg_post + log(median_income)
                  , data = grouped_vals)
  reg_pop_dens = lm(diff_dep~mean_reg_post + log(pop_dens)
                    , data = grouped_vals)
  reg_unemployment = lm(diff_dep~mean_reg_post + diff_unemployment,
                        data = grouped_vals)
  reg_no_controls = lm(diff_dep~mean_reg_post, data = grouped_vals)
  
  return_var = 
    list("reg_all_controls" = summary(reg_all_controls),
         "reg_income" = summary(reg_income),
         "reg_pop_dens" = summary(reg_pop_dens),
         "reg_unemployment" = summary(reg_unemployment),
         "reg_no_controls" = summary(reg_no_controls),
         "dataframe" = grouped_vals,
         "tex" = texreg(list(reg_all_controls,
                             reg_income,
                             reg_pop_dens,
                             reg_unemployment,
                             reg_no_controls),
                        include.ci = FALSE,
                        custom.coef.names=c("Intercept", "Covid Prevalence",
                             "log(Median Income)", "log(Pop. Dens.)",
                             "Unemployment")))
  print("Done.")
  return(return_var)
}
#Example function call
#test_results = compare_pre_post_covid(trimmed_data, "depr_agg", "covid_prev")
