rm(list=ls())
library(xtable)
library(ggplot2)
options(digits=3)


year.set = c(1980, 1990, 2000, 2005, 2010, 2015)

out_table = matrix(NA, length(year.set), 6)

# Read data
for (i in c(1:length(year.set))){
  year = year.set[i]
  cat("Reading Data in ", year.set[i], "....\n")
  data_ipums = read.csv(file=paste0('../data/data_ipums_',year,'.csv'))
  n_obs = nrow(data_ipums)
  mean_educ = mean(data_ipums$EDU)
  mean_educ_male = mean( data_ipums[data_ipums$FEM == 0, 'EDU'])
  mean_educ_female = mean( data_ipums[data_ipums$FEM == 1, 'EDU'])
  mean_female = mean(data_ipums$FEM)
  out_table[i,] = 
    c(format(year.set[i], nsmall=0),
      format(n_obs, nsmall=0), 
      format(mean_female, nsmall=3),
      format(mean_educ, nsmall=3), 
      format(mean_educ_male, nsmall=3),
      format(mean_educ_female, nsmall=3)
    )
}  
  

colnames(out_table) = c("Year", "Sample Size", "Mean($Female$)", "Mean($Educ$)", "Mean($Educ|Male$)", "Mean($Educ|Female$)")
print(xtable(out_table, caption = paste0("Summary Statistics"), 
             type="latex", digits=c(0,0,0,3,3,3,3), align=rep("c",7)), include.rownames=FALSE, file=paste0("../result/table_04.tex"))
