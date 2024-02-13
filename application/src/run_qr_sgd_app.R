# Last update: 2024-02-13
# Before running this code, you need to save the data file in the "data" folder.
# The link to the data file can be found at: 
#   https://github.com/SGDinference-Lab/replication_qr_sgd
# 

rm(list=ls())
library(dplyr, warn.conflicts = FALSE)
library(fastDummies)
library(tictoc)
library(quantreg)
library(conquer)
library(SGDinference)
source("fn_estimate_model.R")

# Qunatile Regression
# Model
# ln(wage) = bt0 + bt1*FEM + bt2*EDU + bt3*(FEM*EDU) + bt4'X + u
#   Design 2
#   X: 5 year intervals of age, State dummies and their interactions with FEM
#

# Set a seed number
seed = 38914
set.seed(seed)

# Set parameters

# For slurm input in the cluster system
# args = commandArgs(trailingOnly = TRUE)
# i = as.numeric(args[1])

# 'i' is an index for year
# i=1 -> year 1980
# i=2 -> year 1990
# i=3 -> year 2000
# ...
# Set i for the specific year you want to estimate
i = 1
year.set = c(1980, 1990, 2000, 2005, 2010, 2015)
qt.set = seq(0.1, 0.9, 0.1)
method="SGDInference"
age_threshold = seq(17.9,65.9,4)
n_age = length(age_threshold)

# Read data
if (i <= 3){
  # YEAR 1980, 1990, 2000
  year = year.set[i]
  tic("Loading Data : ")
  data_ipums = read.csv(file=paste0('../data/data_ipums_',year,'.csv'))
  toc() # time: < 40 sec
  n_obs = nrow(data_ipums)

  # Construct additional regressors
  tic("Construct Additional Regressors : ")
  data_ipums = data_ipums %>% mutate(age_factor = cut(data_ipums$AGE, age_threshold))
  data_ipums <- dummy_cols(data_ipums, select_columns = 'age_factor')
  data_ipums = data_ipums[, -c(6:15,16,68)]  # Drop 8 regional dummies, AGE, and AGESQ, STATEFIP_1, AGE[18,22)
  data_ipums = data_ipums[, -c(56)]    # Drop age_factor variable (NOT dummy)
  colnames(data_ipums) = c("YEAR", "LWAGE", "FEM", "EDU", "FEM_EDU", paste0("S",c(1:50)), paste0("A",c(1:11)))
  # S1: Alaska
  # A1: [22,26)
  
  # Generate interaction terms
  S_sum = paste(paste0("S",c(1:50)), collapse="+")
  A_sum = paste(paste0("A",c(1:11)), collapse="+")
  fmla =  as.formula(paste("~ FEM * (", S_sum, ") * (", A_sum, ")"))
  temp = model.matrix(fmla, data=data_ipums)
  temp = temp[, -c(1:63)]

  data_ipums = cbind(as.matrix(data_ipums), temp)
  rm(temp)
  gc()
  toc() 
  
  # Pre-processing the dataset
  n0 = n_obs*0.01 # subset size for delta_0 and studentization parameters
  sub_index = sample(n_obs,n0)
  # Compute the sd of y for gamma_0
  sig_hat = sd(data_ipums[sub_index,2])

   for (j in c(1:9)){
    qt = qt.set[j]
    gamma_0 = dnorm(qnorm(qt))/sqrt(qt*(1-qt))/sig_hat
    # Compute the results
    out_sub = estimate_model(data_ipums, qt, method, gamma_0, studentize=F, path=F)    

    filename = paste0("../result/method_",method,"_",year,"_",qt,"_03.RData")
    # Print out and save the results
    cat("Method : ", method,'\n')
    cat("Year   : ", year,'\n')
    cat("qt     : ", qt,'\n')
    cat("n_obs  : ", n_obs,'\n')
    cat("Time  : ", out_sub$time,'\n')
    save(out_sub, file=filename)
    gc()
  }
} else { 
  # YEAR 2005, 2010, 2015
  year = year.set[i]
  tic("Loading Data : ")
  data_ipums = read.csv(file=paste0('../data/data_ipums_',year,'.csv'))
  toc()
  n_obs = nrow(data_ipums)
  
  # Consturcting Regressors
  tic("Construct Additional Regressors : ")
  data_ipums = data_ipums %>% mutate(age_factor = cut(data_ipums$AGE, age_threshold))
  data_ipums <- dummy_cols(data_ipums, select_columns = 'age_factor')
  data_ipums <- dummy_cols(data_ipums, select_columns = 'YEAR')
  data_ipums = data_ipums[, -c(6:15,16,68, 80)]  # Drop 8 regional dummies, AGE, and AGESQ, STATEFIP_1, AGE[18,22), first YEAR dummy (year-4)
  data_ipums = data_ipums[, -c(56)]    # Drop age_factor variable (NOT dummy)
  colnames(data_ipums) = c("YEAR", "LWAGE", "FEM", "EDU", "FEM_EDU", paste0("S",c(1:50)), paste0("A",c(1:11)), paste0("YEAR",seq(year-3,year,1)))
  # S1: Alaska
  # A1: [22,26)
  
  # Generate interaction terms
  S_sum = paste(paste0("S",c(1:50)), collapse="+")
  A_sum = paste(paste0("A",c(1:11)), collapse="+")
  fmla =  as.formula(paste("~ FEM * (", S_sum, ") * (", A_sum, ")"))
  temp = model.matrix(fmla, data=data_ipums)
  temp = temp[, -c(1:63)]
  
  data_ipums = cbind(as.matrix(data_ipums), temp)
  rm(temp)
  gc()
  toc()
  
  # Pre-processing the dataset
  n0 = n_obs*0.01 # subset size for delta_0 and studentization parameters
  sub_index = sample(n_obs,n0)
  # Compute the sd of y for gamma_0
  sig_hat = sd(data_ipums[sub_index,2])
  
  for (j in c(1:9)){
    qt = qt.set[j]
    gamma_0 = dnorm(qnorm(qt))/sqrt(qt*(1-qt))/sig_hat
    # Compute the results
    out_sub = estimate_model(data_ipums, qt, method, gamma_0, studentize=F, path=F)    

    filename = paste0("../result/method_",method,"_",year,"_",qt,"_03.RData")
    # Print out and save the results
    cat("Method : ", method,'\n')
    cat("Year   : ", year,'\n')
    cat("qt     : ", qt,'\n')
    cat("n_obs  : ", n_obs,'\n')
    cat("Time  : ", out_sub$time,'\n')
    save(out_sub, file=filename)
    gc()
  }
}

