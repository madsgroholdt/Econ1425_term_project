#This file is intended to explore various composite variables created
#from the survey data

library(plyr)
require(dplyr)

#vennkont: Are diff avenues of contact with friends substitutes or complements?
friend_reg = lm(vennkont1~vennkont2 + vennkont3 + vennkont4 + vennkont5,
                data=data[data$kjønn == 1])
print(summary(friend_reg))

#Function for regressing composite variables on every variable in the dataset
#Takes a composite variable and returns the stat sig variables in the dataset
extra_interesting_corr_df = data.frame(matrix(ncol = 4, nrow = 0))
colnames(extra_interesting_corr_df) = c("x_name", "y_name",
                                        "correlation", "p_value")

comp_var_regression = function(dep_var_ind) {
  interesting_df = data.frame(matrix(ncol = 4, nrow = 0))
  colnames(interesting_df) = c("x_name", "y_name",
                               "correlation", "p_value")
  for (i in 1:ncol(ultra_trimmed_data)) {
    if (as.numeric(nrow(
          ultra_trimmed_data[
            !is.na(ultra_trimmed_data[,dep_var_ind]) &
            !is.na(ultra_trimmed_data[,i]),])) > 0) {
      reg = 
        lm(unlist(ultra_trimmed_data[,dep_var_ind])~unlist(ultra_trimmed_data[,i]),
           data=ultra_trimmed_data)
      reg_summary = summary(reg)
    }
    if(ncol(reg_summary$coefficients) >= 4 &
       nrow(reg_summary$coefficients) >= 2) {
        interesting_df[i,] = 
          c(colnames(ultra_trimmed_data)[dep_var_ind],
            colnames(ultra_trimmed_data)[i],
            reg_summary$coefficients[2, 1],
            reg_summary$coefficients[2, 4]) 
    }
    if (i %% 25 == 0) {
      print(cat("Done with column: ", i))
    }
  }
  interesting_df = interesting_df[!is.na(interesting_df$correlation),]
  remove_ind = c()
  for (j in 2:nrow(interesting_df)) {
    if (interesting_df[j, "correlation"] ==
        interesting_df[j - 1, "correlation"]) {
      remove_ind = append(remove_ind, colnames(ultra_trimmed_data)[j])
    }
  }
  print(remove_ind)
  interesting_df = within(interesting_df, rm(list = remove_ind))
  interesting_df = interesting_df[interesting_df$p_value < 0.2,]
  
  return(interesting_df)
}
#Run this if you find the output from the above function interesting enough
#to be part of a permanent collection of special variables
extra_interesting_corr_df = rbind(extra_interesting_corr_df, FUNCTION NAME)

#Regress aggregate school trouble variable on all other variables
school_trouble = 
  comp_var_regression(trimmed_data[,"skolvansk_aggregate"],
                      "skolvansk_aggregate")
  
#Regress average substance abuse variable on all other variables
alc_and_drug_abuse = comp_var_regression(
  grep("alc_and_drug_agg", colnames(ultra_trimmed_data)))

#Substance abuse on neighborhood amenities
grouped_by_grade = 
  ultra_trimmed_data %>% group_by(mean_amenities = round(neigh_amenities, digits = 0)) %>%
  summarise(mean_alc = mean(alc_and_drug_agg, na.rm = TRUE),
            mean_søs = mean(søs, na.rm = TRUE))

summary(lm(mean_alc~mean_amenities + factor(round(mean_søs, digits = 2)),
           data = grouped_by_grade))
summary(lm(alc_and_drug_agg~neigh_amenities, data = trimmed_data),)

ggplot(grouped_by_grade, aes(x = -mean_amenities, y = mean_alc)) + 
  geom_point() + geom_smooth(method = "lm", se = FALSE)







#fixest package to use fixed effects
feols(depr_agg~covid_prev | år,
      data = trimmed_data[trimmed_data$år %in% c(2020, 2021),], se = "hetero")

library(fixest)
library(lfe)

summary(felm(depr_agg~covid_prev | år | 0 | kommune,
             data = trimmed_data[trimmed_data$år %in% 2020:2021,]))


summary(lm(depr_agg~ factor(år),
           data = trimmed_data[trimmed_data$år %in% 2010:2021,]))

