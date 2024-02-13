# Date: 2024-02-12
# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

rm(list=ls())

library(dplyr, warn.conflicts = FALSE)
library(ipumsr)
library(fastDummies)
library(here)

# Set <Working Directory> as the location of this source file (data_cleaning.R)
setwd(here())

# Check if relevant data source files exist. If not, download it from the data repository.
if (!file.exists('PCEPI.csv')||
    !file.exists('usa_00005.xml')||
    !file.exists('usa_00005.cbk')||
    !file.exists('usa_00005.cbk')
    ){
  cat("Download the Data.zip file from the link in the README file. \n")
}


# Read the original data file and save it as data
ddi <- read_ipums_ddi("usa_00005.xml")
data <- read_ipums_micro(ddi)

#---------------------------------------
# Data cleaning
#---------------------------------------
# (1) RACE = White
# (2) AGE in [18, 65]
# (3) INCWAGE > 0
# (4) WKSWORK1 > 0 or WKSWORK2 > 0
# (5) UHRSWORK >= 0
# The working data file name: data_ipums

data_ipums = data %>% filter(RACE==1 & AGE>=18 & AGE <=65 & INCWAGE > 0)
data_ipums = data_ipums %>% 
  filter(WKSWORK1 > 0 | WKSWORK2 > 0) %>%
  filter(UHRSWORK >= 30)
data_ipums = data_ipums %>% filter(YEAR != 2013) # Drop YEAR=2013 data

# Compute the implied weekly earnings
# WKSWORK2            Weeks worked last year, intervalled
# 0                   N/A
# 1                   1-13 weeks      ---> 7
# 2                   14-26 weeks     ---> 20
# 3                   27-39 weeks     ---> 33
# 4                   40-47 weeks     ---> 43.5
# 5                   48-49 weeks     ---> 48.5
# 6                   50-52 weeks     ---> 51
data_ipums = data_ipums %>% mutate(WKSWORK3 = 7*(WKSWORK2==1) + 20*(WKSWORK2==2) + 33*(WKSWORK2==3) +
                                     43.5*(WKSWORK2==4) + 48.5*(WKSWORK2==5) + 51*(WKSWORK2==6))

data_ipums = data_ipums %>% 
  mutate(WKEARN = if_else(YEAR <= 2005, (INCWAGE / WKSWORK1), (INCWAGE / WKSWORK3)) ) 

data_ipums = data_ipums %>% 
  mutate(YEAR = if_else(YEAR <= 2005, YEAR, as.integer(MULTYEAR) ) ) 


# filter if WKEARN >= 62 (a half of minimum wage in 1980 ($3.10 * 40 hours = $62))
lb_WKEARN = 3.10*40/2
data_ipums = data_ipums %>% filter(WKEARN > lb_WKEARN)

#------------------------------------------------------------------
# Generate female dummy
#
# FEM = (SEX == 2)
#------------------------------------------------------------------
data_ipums = data_ipums %>% mutate(FEM = SEX == 2)


#------------------------------------------------------------------
# Generate education dummies
#  EDU: Some College or above
#------------------------------------------------------------------
data_ipums = data_ipums %>% mutate(EDU = EDUCD >= 65)


#------------------------------------------------------------------
# Generate the interaction of FEM and EDU
#------------------------------------------------------------------
data_ipums = data_ipums %>% mutate(FEM_EDU = FEM*EDU)

#------------------------------------------------------------------
# Generate region dummies
#
# (1) R1: New England         Division (REGION == 11)
# (2) R2: Middle Atlantic     Division (REGION == 12)
# (3) R3: East North Central  Division (REGION == 21)
# (4) R4: West North Central  Division (REGION == 22)
# (5) R5: South Atlantic      Division (REGION == 31)
# (6) R6: East South Central  Division (REGION == 32)
# (7) R7: West South Central  Division (REGION == 33)
# (8) R8: Mountain            Division (REGION == 41)
# (9) R9: Pacific             Division (REGION == 42) [BASE GROUP]
#------------------------------------------------------------------
data_ipums = data_ipums %>% mutate(R1 = REGION == 11) %>%
  mutate(R2 = REGION == 12) %>%
  mutate(R3 = REGION == 21) %>%
  mutate(R4 = REGION == 22) %>%
  mutate(R5 = REGION == 31) %>%
  mutate(R6 = REGION == 32) %>%
  mutate(R7 = REGION == 33) %>%
  mutate(R8 = REGION == 41) %>%
  mutate(R9 = REGION == 42)


# Generate STATE dummy variables using the fastDummies package
data_ipums <- dummy_cols(data_ipums, select_columns = 'STATEFIP')

#---------------------------------------------------------------
# Construct price indices for each year
#---------------------------------------------------------------
pce_data = read.csv("PCEPI.csv", header=TRUE) %>%
  filter(DATE=="1980-01-01" | DATE=="1990-01-01" | 
           DATE=="2000-01-01" | DATE=="2001-01-01" |
           DATE=="2002-01-01" | DATE=="2003-01-01" |
           DATE=="2004-01-01" | DATE=="2005-01-01" |
           DATE=="2006-01-01" | DATE=="2007-01-01" |
           DATE=="2008-01-01" | DATE=="2009-01-01" |
           DATE=="2010-01-01" | DATE=="2011-01-01" |
           DATE=="2012-01-01" | DATE=="2013-01-01" |
           DATE=="2014-01-01" | DATE=="2015-01-01") %>%
  mutate(P_index = PCEPI / PCEPI[1])
data_ipums = data_ipums %>% mutate(P_IDX = (YEAR==1980)*pce_data[1,3] +
                                     (YEAR==1990)*pce_data[2,3] +
                                     (YEAR==2000)*pce_data[3,3] +
                                     (YEAR==2001)*pce_data[4,3] +
                                     (YEAR==2002)*pce_data[5,3] +
                                     (YEAR==2003)*pce_data[6,3] +
                                     (YEAR==2004)*pce_data[7,3] +
                                     (YEAR==2005)*pce_data[8,3] +
                                     (YEAR==2006)*pce_data[9,3] +
                                     (YEAR==2007)*pce_data[10,3] +
                                     (YEAR==2008)*pce_data[11,3] +
                                     (YEAR==2009)*pce_data[12,3] +
                                     (YEAR==2010)*pce_data[13,3] +
                                     (YEAR==2011)*pce_data[14,3] +
                                     (YEAR==2012)*pce_data[15,3] +
                                     (YEAR==2013)*pce_data[16,3] +
                                     (YEAR==2014)*pce_data[17,3] +
                                     (YEAR==2015)*pce_data[18,3]) 

#----------------------------------------------------------------
# Construct additional variables
#----------------------------------------------------------------
data_ipums = data_ipums %>%
  mutate(AGESQ = AGE^2) %>%
  mutate(LWAGE = log(WKEARN/P_IDX))


#----------------------------------------------------------------
# Select necessary variables
#----------------------------------------------------------------
state_id = c(1:2,4:6,8:13,15:42,44:51,53:56) # 51 variables including DC
data_ipums = data_ipums %>% dplyr::select(YEAR, LWAGE, FEM, EDU, FEM_EDU, paste0("R",c(1:8)), AGE, AGESQ, paste0("STATEFIP_",state_id))

#-----------------------------------------------------------------------
# Randomize the data order just in case there is any unexpected sorting
#-----------------------------------------------------------------------
seed = 38914
set.seed(seed)

n_obs = nrow(data_ipums)
data_ipums$rnd = runif(n_obs)
data_ipums = data_ipums[order(data_ipums$rnd),] %>% dplyr::select(-c(rnd))


#------------------------------------------------------------------
# Save the data files in the csv format
#------------------------------------------------------------------
write.csv(subset(data_ipums, YEAR==1980), file="data_ipums_1980.csv", row.names=FALSE)
write.csv(subset(data_ipums, YEAR==1990), file="data_ipums_1990.csv", row.names=FALSE)
write.csv(subset(data_ipums, YEAR==2000), file="data_ipums_2000.csv", row.names=FALSE)
write.csv(subset(data_ipums, (YEAR>=2001) & (YEAR<=2005)), file="data_ipums_2005.csv", row.names=FALSE)
write.csv(subset(data_ipums, (YEAR>=2006) & (YEAR<=2010)), file="data_ipums_2010.csv", row.names=FALSE)
write.csv(subset(data_ipums, (YEAR>=2011) & (YEAR<=2015)), file="data_ipums_2015.csv", row.names=FALSE)
