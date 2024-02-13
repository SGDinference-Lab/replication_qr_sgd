rm(list=ls())
library(dplyr)
library(xtable)
library(ggplot2)
options(digits=3)
options(scipen=999)

method = "SGDInference"
n_set = c(3659684,
          4192119,
          4479724,
          2493787,
          4708119,
          4542874
)
year_set = c(1980,1990,2000,2005,2010,2015)
qt_set = seq(0.1,0.9,0.1)

n_year = length(year_set)
n_qt = length(qt_set)
for (i.qt in c(1:n_qt)){
  qt = qt_set[i.qt]
# Set parameters
#args = commandArgs(trailingOnly = TRUE)
#qt = as.numeric(args[1]) # qt: 0.1, 0.2, ..., 0.9
#qt = 0.9

# Read the result data sets
bt_male = bt_female = bt_diff = rep(NA,n_year)
se_male = se_female = se_diff = rep(NA,n_year)
ci_male = ci_female = ci_diff = matrix(NA, n_year, 2)

out_table = matrix(NA, 2*n_year, 5)
for (i in c(1:n_year)){
  read_filename = paste0("../result/method_", method,"_",year_set[i],"_",qt,"_03.RData")
  load(read_filename)
  # bt: college wage premiums
  bt_male[i] = out_sub$bt.hat[3]
  bt_female[i] = out_sub$bt.hat[3] + out_sub$bt.hat[4]
  bt_diff[i] = out_sub$bt.hat[4]
  # Compute standard errors
  se_male[i] = sqrt(out_sub$Sigma.hat[1,1] / n_set[i])
  se_female[i] = sqrt(c(1,1) %*% (out_sub$Sigma.hat) %*% c(1,1) / n_set[i])
  se_diff[i] = sqrt(out_sub$Sigma.hat[2,2] / n_set[i])
  # Compute 95% confidence intervals
  ci_male[i,] = cbind( bt_male[i] - 6.747*se_male[i], bt_male[i] + 6.747*se_male[i])
  ci_female[i,] = cbind( bt_female[i] - 6.747*se_female[i], bt_female[i] + 6.747*se_female[i])
  ci_diff[i,] = cbind( bt_diff[i] - 6.747*se_diff[i], bt_diff[i] + 6.747*se_diff[i])
  # Computeation time
  time = out_sub$time
  
  # Generate the table
  # construct 95% CI table
  out_table[(2*i-1), ] = c(       format(year_set[i], nsmall=0),
                                  format(round(bt_female[i],4), nsmall=4), 
                                  format(round(bt_male[i],4), nsmall=4), 
                                  format(round(bt_diff[i],4), nsmall=4), 
                                  format(round(time,2),nsamll=2)
  )
  out_table_i = c( NA,
                   paste0('[',format(round(ci_female[i,1],4), nsmall=4),',',format(round(ci_female[i,2],4), nsmall=4),']' ),
                   paste0('[',format(round(ci_male[i,1],4), nsmall=4),',',format(round(ci_male[i,2],4), nsmall=4),']' ),
                   paste0('[',format(round(ci_diff[i,1],4), nsmall=4),',',format(round(ci_diff[i,2],4), nsmall=4),']' ),
                   NA
  )
  out_table[(2*i), ] = out_table_i
}

colnames(out_table) = c("Year", "Female", "Male", "Difference", "Time (sec.)")
print(xtable(out_table, caption = paste0("Quantile College Premium $\\tau=",qt,"$"), 
             type="latex", digits=c(0,0,4,4,4,2), align=rep("c",6)),include.rownames=FALSE, file=paste0("../result/table/table_",qt,"_03.tex"))

# Construct the data.frame for Chart 1: College Wage Premium for Male and Female
df_male = data.frame(cbind(year_set, bt_male)) %>% mutate(Sex = "Male")
df_female = data.frame(cbind(year_set, bt_female)) %>% mutate(Sex = "Female") 
colnames(df_male) = colnames(df_female) = c("Year", "Premium", "Sex")
df_chart_01 = rbind(df_male, df_female)

if (qt == 0.1){
  chart_01 = 
    df_chart_01 %>%
    ggplot( aes(x=Year, y=Premium, group=Sex)) +
    geom_line(aes(linetype=Sex, color=Sex), size=1.5) +
    scale_linetype_manual(values=c("twodash", "solid"))+
    scale_color_manual(values=c('darkred','darkblue'))+
    ggtitle(paste0("Quantile = ", qt)) +
    ylab("College Premium") +
    scale_y_continuous(limits = c(0, 0.6)) +
    theme(text = element_text(size = 30)) +  
    theme(legend.key.size = unit(1.5, 'cm')) +
    theme(legend.title = element_blank(), legend.position = c(0.15, 0.85))
} else {
chart_01 = 
  df_chart_01 %>%
    ggplot( aes(x=Year, y=Premium, group=Sex)) +
    geom_line(aes(linetype=Sex, color=Sex), size=1.5) +
    scale_linetype_manual(values=c("twodash", "solid"))+
    scale_color_manual(values=c('darkred','darkblue'))+
    ggtitle(paste0("Quantile = ", qt)) +
    ylab("College Premium") +
    scale_y_continuous(limits = c(0, 0.6)) +
    theme(text = element_text(size = 30)) +  
    theme(legend.position="none")
}
png( file=paste0("../result/chart/chart_",qt,"_03.png"), width = 600, height = 600)
print(chart_01)
dev.off()


# Construct the data.frame for Chart 2: College Wage Premium Differences
df_diff = data.frame(cbind(year_set, bt_diff))
df_diff = cbind(df_diff, ci_diff)
colnames(df_diff) = c("Year", "Diff", "ci_lb", "ci_ub")
  chart_02 = df_diff %>%
    ggplot( aes(x=Year, y=Diff)) +
    geom_ribbon(aes(ymin = ci_lb, ymax = ci_ub), fill = "grey70") +
    geom_line(linewidth=1.5) + 
    ggtitle(paste0("Quantile = ", qt)) +
    scale_y_continuous(limits = c(-0.2, 0.2)) +
    theme(text = element_text(size = 30)) +  
    ylab("College Premium Difference") +
    theme(legend.key.size = unit(1.5, 'cm')) +
    theme(legend.position="none")

png( file=paste0("../result/chart/diff_chart_",qt,"_03.png"), width = 600, height = 600)
print(chart_02)
dev.off()
    

}
#write.csv(round(ci_table,3), file="table_ci.out")

