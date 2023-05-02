#This file is intended for general between-county
#observations and comparisons, both fixed-time and not

between_county_diff = function(main_df, int_var) {
  agg_df = main_df %>%
    group_by(fylke) %>%
    summarise(mean_var_of_int = mean(int_var))
  agg_df = agg_df %>% as.data.frame()
  return(agg_df)
}

between_county_diff(trimmed_data[,c("fylke", "skolvansk_aggregate")],
                    "skolvansk_aggregate")

test_tbl =  trimmed_data[,c("fylke", "skolvansk_aggregate")] %>% 
  group_by(fylke) %>% 
  summarise(mean_skolvansk=mean(skolvansk_aggregate, na.rm = TRUE))

print(summary(lm(alc_and_drug_agg~neigh_amenities + factor(round(søs, digits=0)),
                 data = trimmed_data)))


between_county_test = between_county_diff(trimmed_data, "alc_and_drug_agg")

#Between county variation in alc and drug consumption
#TODO: Abstract this into a function to allow for repeat generation
library(plyr)
require(dplyr)
library("ggplot2") 
#Development in substance use in different counties over the time period
alc_and_drug_by_county_and_year = 
  trimmed_data %>%
  group_by(år) %>%
  dplyr::summarise(school_enjoyment = mean(school_enjoyment_agg, na.rm = TRUE))

ggplot(alc_and_drug_by_county_and_year,
       aes(x = år,
           y = school_enjoyment)) + geom_line()

#Substacne use by county
alc_and_drug_by_county = trimmed_data %>%
  group_by(county) %>%
  summarise(mean_alc_agg = mean(alc_and_drug_agg, na.rm = TRUE),
            mean_relig_beliefs = mean(relig2, na.rm = TRUE))

ggplot(alc_and_drug_by_county,
       aes(x=-mean_relig_beliefs, y=mean_alc_agg)) +
  geom_point() + geom_text(label=alc_and_drug_by_county$county)

#Testing importance of religious beliefs on alcohol consumption
summary(lm(alc_and_drug_agg~søs + relig2 + neigh_amenities, data=trimmed_data))

#Difference between development in political vs. non-political youth following Utøya
#The biggest terrorist attack in Norway in modern history, targeted at youth political camp
trimmed_data$angst_agg = trimmed_data$angst1 + trimmed_data$angst2 + trimmed_data$angst4

trimmed_data <- within(trimmed_data, {   
  poldelt1.cat <- NA # need to initialize variable
  poldelt1.cat[poldelt1 == 1] <- "Aktiv"
  poldelt1.cat[poldelt1 == 2] <- "Inaktiv"
} )

anxiety_over_time = 
  trimmed_data %>% group_by(år, county) %>%
  summarise(mean_anxiety = mean(angst_agg, na.rm = TRUE),
            total_count=n(),
            .groups = 'drop')

anxiety_agg_over_time = trimmed_data %>%
  group_by(år) %>% summarise(mean_anxiety = mean(angst_agg, na.rm = TRUE))

ggplot(anxiety_agg_over_time[anxiety_agg_over_time$år != c(2010, 2011, 2012, 2013),],
       aes(x = år, y = mean_anxiety)) + geom_point()
#+ geom_text(label=anxiety_over_time$county)


#getting along with family/friends over time
covid_activity_by_county =
  trimmed_data %>% group_by(county) %>% 
  summarise(mean_activity_change = mean(covid5, na.rm = TRUE))


