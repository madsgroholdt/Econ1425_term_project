#This file has code for general data exploration. Finds interesting
#correlations alghoritmically

#Generate correlation matrix for all variables
#THIS TAKES 1+ MINUTES

cor_matrix = cor(trimmed_data, use = "pairwise.complete.obs")
print(cor_matrix)

#Get 5th and 95th quantile of cor_matrix
quants = quantile(cor_matrix, probs = c(0.05, 0.95), na.rm = TRUE)

#Find most prominent correlation relationships
rows = c()
columns = c()
rownames = c()
columnnames = c()
correlation_vec = c()

for (i in 1:ncol(cor_matrix)) {
  if (i %% 10 == 0) {
    print(cat("Working on column", i)) 
  }
  for (j in 1:nrow(cor_matrix)) {
    if (!is.na(cor_matrix[j, i])) {
      if ((cor_matrix[j, i] < quants[1]) | 
          (cor_matrix[j, i] > quants[2])) {
        rows = append(rows, j)
        columns = append(columns, i)
        rownames = append(rownames, row.names(cor_matrix)[j])
        columnnames = append(columnnames, colnames(cor_matrix)[i])
        correlation_vec = append(correlation_vec, cor_matrix[j, i])
      }
    }
    
  }
}

#df with most prominent correlations, the variable names, indices, and stat sig
strong_cor_df = data.frame(columns, rows, columnnames, rownames, correlation_vec)
strong_cor_df = strong_cor_df[
  (strong_cor_df$columns != strong_cor_df$rows) &
  !(strong_cor_df$columnnames %in% 
  c("skolenivå", "fylke", "kommune", "kjønn", "klasse")) &
  !(strong_cor_df$rownames %in% 
  c("skolenivå", "fylke", "kommune", "kjønn", "klasse")),]

#Simple linear regression of each of the strongly correlated variables
stat_sig = c()
reg_coeff = c()
reg_const = c()

#Runtime = 4-5hrs. 
#This will generate basic regression data for all variables that
#showed a strong correlation with each other
#Have to trim away clearly related variables afterwards, but idea
#is to find a list of somewhat strongly related variables
for (i in 1:nrow(strong_cor_df)) {
  dependent = trimmed_data[,strong_cor_df[i,]$columnnames]
  regressor = trimmed_data[,strong_cor_df[i,]$rownames]
  if (i %% 100 == 0) {
    print(cat("Working on row:", i))
  }
  regression = lm(unlist(dependent)~unlist(regressor))
  reg_sum = summary(regression)
  
  stat_sig = append(stat_sig, reg_sum$coefficients[,4])
  reg_coeff = append(reg_coeff, reg_sum$coefficients[2,1])
  reg_const = append(reg_const, reg_sum$coefficients[1,1])
}

#Messed up the stat_sig for the above code, but correct it with this
corrected_stat_sig = c()
for (i in sequence(length(stat_sig) / 2, from = 2, by=2)) {
  corrected_stat_sig = append(corrected_stat_sig, as.numeric(stat_sig[i]))
}

strong_cor_df$stat_sig = corrected_stat_sig
strong_cor_df$reg_coeff = reg_coeff
strong_cor_df$reg_const = reg_const

#Identify cool relationships based on individual regressions as those who
#are correlated, but not directly correlated. Meaning stat sig but not ***
cool_relationships = strong_cor_df[(strong_cor_df$stat_sig > 0.001) &
                                     (strong_cor_df$stat_sig <= 0.1),]
#Order dataframe rows by stat sig
cool_relationships = cool_relationships[order(cool_relationships$stat_sig),]

#Remove all lines where dependent variable and regressor are switched around
remove_ind = c()
for (i in 2:nrow(cool_relationships)) {
  if (cool_relationships[i,]$rownames == 
      cool_relationships[i - 1,]$columnnames) {
    remove_ind = append(remove_ind, i)
  }
}

cool_relationships = cool_relationships[-remove_ind,]

#Find sample size for each regression in cool relationships
sample_size_vec = c()
for (i in 1:nrow(cool_relationships)) {
  col1 = cool_relationships[i,]$columnnames
  col2 = cool_relationships[i,]$rownames
  trim = trimmed_data[,c(col1, col2)]
  sample_size = nrow(trim[!is.na(c(trim[,col1])) &
                            c(!is.na(trim[,col2])),])
  sample_size_vec = append(sample_size_vec, sample_size)
  if (i %% 10 == 0) {
    print(cat("done with row", i))
  }
}
cool_relationships$sample_size = sample_size_vec

#Write any dataframes in R environment to this folder to avoid having to rerun
#time consuming parts of the above code
write.csv(cool_relationships,
  "/Volumes/GoogleDrive/My Drive/Sophomore Spring/Econ 1425/Sports Inclusion/data_files/output/cool_regression_relationships.csv",
          row.names=FALSE)
