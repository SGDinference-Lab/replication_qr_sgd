options(digits=5, scipen = 999)
rm(list=ls())

time.qr=time.sqr.plug=time.sqr.boot=time.sgd.boot=time.sgd.rs=time.sgd.rs1=time.sgd.rs1s=NA
bias.qr=bias.sqr.plug=bias.sqr.boot=bias.sgd.boot=bias.sgd.rs=bias.sgd.rs1=bias.sgd.rs1s=NA


library(MASS)
library(quantreg)
library(conquer)
library(SGDinference)
library(tictoc)
library(Rcpp)

sourceCpp('sgdi_boot_qr.cpp')
source('sgdi_boot_qr.R')

# Parameters
# args[1]: seed
# args[2]: n
# args[3]: p
# args[4]: replications

# Used for direct input in clusters
#args = commandArgs(trailingOnly=TRUE)
#set.seed(as.numeric(args[1]))
#n = as.numeric(args[2])
#p = as.numeric(args[3])
#replications = as.numeric(args[4])

set.seed=1
n=1e05
p=10
replications=10


burn = 0
alpha = 0.501
gamma_0 = 1

eff.n = n - burn

critical.value = 6.747 # From Abadir and Paruolo (1997) Table 1. 97.5%

out.sqr.boot = matrix(NA, replications, 4)
colnames(out.sqr.boot) = c("Coverage", "CI Length", "Runtime", "Bias")
out.qr = out.sqr.plug = out.sgd.boot = out.sgd.rs = out.sgd.rs1s = out.sqr.boot

filename = paste0("p",p,"/n1e0",log10(n),"/","seed",set.seed)
sink(paste0(filename,".out"))

cat('------------------------ \n')
cat('Sample Size = ', n, '\n')
cat('dim of X    = ', p, '\n')
cat('# of rep    = ', replications, '\n')
cat('------------------------ \n')

for (i.rep in (1:replications)){
  tic()
  cat("Replications : ",i.rep,"\n")
#  sink()
#  cat("Replications : ",i.rep,"\n")    
#  sink(paste0(filename,".out"), append = TRUE)

  # DGP
  beta = rep(1, p + 1)
  # beta = c(1,1/c(1:p))
  X = matrix(rnorm(n*p), n, p)
  err = rnorm(n)
  Y = cbind(1, X) %*% beta + err
  tau = 0.5
  
  gc(reset=TRUE)
  
  # 1. qr: quantile package
  start.qr = Sys.time()
  fit.qr = rq(Y ~ X, tau = tau, method = "pfn")
  inf.qr = summary(fit.qr, se="nid")
  lower.qr = inf.qr$coef[2,1] - 1.96 * inf.qr$coef[2,2]
  upper.qr = inf.qr$coef[2,1] + 1.96 * inf.qr$coef[2,2]
  end.qr = Sys.time()
  time.qr = as.numeric(difftime(end.qr, start.qr, units = "secs"))
  bias.qr = fit.qr$coefficients[2] - beta[2]
  coverage.qr = (lower.qr <= beta[2]) & (upper.qr >= beta[2])
  length.qr = upper.qr - lower.qr
  out.qr[i.rep, ] = c(coverage.qr, length.qr, time.qr, bias.qr)

  gc(reset=TRUE)
  
  # 2. sqr.plug (plug-in asymptotic variance): conquer package
  start.sqr.plug = Sys.time()
  fit.sqr.plug = conquer(X, Y, tau = tau, ci="asymptotic")
  lower.sqr.plug = fit.sqr.plug$asyCI[2,1]
  upper.sqr.plug = fit.sqr.plug$asyCI[2,2]
  end.sqr.plug = Sys.time()
  time.sqr.plug = as.numeric(difftime(end.sqr.plug, start.sqr.plug, units = "secs"))
  bias.sqr.plug = fit.sqr.plug$coeff[2] - beta[2]
  coverage.sqr.plug = (lower.sqr.plug <= beta[2]) & (upper.sqr.plug >= beta[2])
  length.sqr.plug = upper.sqr.plug - lower.sqr.plug
  out.sqr.plug[i.rep, ] = c(coverage.sqr.plug, length.sqr.plug, time.sqr.plug, bias.sqr.plug)

  # 3. sqr.boot (bootstrap inference): conquer package
  start.sqr.boot = Sys.time()
  fit.sqr.boot = conquer(X, Y, tau = tau, ci="bootstrap")
  lower.sqr.boot = fit.sqr.boot$perCI[2,1]
  upper.sqr.boot = fit.sqr.boot$perCI[2,2]
  end.sqr.boot = Sys.time()
  time.sqr.boot = as.numeric(difftime(end.sqr.boot, start.sqr.boot, units = "secs"))
  bias.sqr.boot = fit.sqr.boot$coeff[2] - beta[2]
  coverage.sqr.boot = (lower.sqr.boot <= beta[2]) & (upper.sqr.boot >= beta[2])
  length.sqr.boot = upper.sqr.boot - lower.sqr.boot
  out.sqr.boot[i.rep, ] = c(coverage.sqr.boot, length.sqr.boot, time.sqr.boot, bias.sqr.boot)
  
  gc(reset=TRUE)

  # 4. sgd.boot (SGD bootstrap method)
  start.sgd.boot = Sys.time()
  fit.sgd.boot = sgdi_boot_qr(X, Y)
  lower.sgd.boot = quantile(fit.sgd.boot$bar_coef_boot_mat[2,], 0.025)
  upper.sgd.boot = quantile(fit.sgd.boot$bar_coef_boot_mat[2,], 0.975)
  end.sgd.boot = Sys.time()
  time.sgd.boot = as.numeric(difftime(end.sgd.boot, start.sgd.boot, units = "secs"))
  bias.sgd.boot = fit.sgd.boot$beta_hat[2] - beta[2]
  coverage.sgd.boot = (lower.sgd.boot <= beta[2]) & (upper.sgd.boot >= beta[2])
  length.sgd.boot = upper.sgd.boot - lower.sgd.boot
  out.sgd.boot[i.rep, ] = c(coverage.sgd.boot, length.sgd.boot, time.sgd.boot, bias.sgd.boot)
  
  gc(reset=TRUE)
  

  # 6. sgd.rs1s (SGD random scaling, single element of V_hat with conquer start)
  start.sgd.rs1s = Sys.time()
  n_s = floor(0.05*n)
  bt_start = conquer(X[c(1:n_s),], Y[c(1:n_s)], tau = tau)$coeff
  fit.sgd.rs1s = sgdi_qr(Y~X, gamma_0 = gamma_0, bt_start = bt_start, burn=burn, alpha=alpha, inference = "rss", rss_idx=1)
  sigma.sgd.rs1s = sqrt(fit.sgd.rs1s$V_hat1/eff.n)
  lower.sgd.rs1s = fit.sgd.rs1s$beta_hat[2] - 6.747 * sigma.sgd.rs1s
  upper.sgd.rs1s = fit.sgd.rs1s$beta_hat[2] + 6.747 * sigma.sgd.rs1s
  end.sgd.rs1s = Sys.time()
  time.sgd.rs1s = as.numeric(difftime(end.sgd.rs1s, start.sgd.rs1s, units = "secs"))
  bias.sgd.rs1s = fit.sgd.rs1s$coefficients[2] - beta[2]
  coverage.sgd.rs1s = (lower.sgd.rs1s <= beta[2]) & (upper.sgd.rs1s >= beta[2])
  length.sgd.rs1s = upper.sgd.rs1s - lower.sgd.rs1s
  out.sgd.rs1s[i.rep, ] = c(coverage.sgd.rs1s, length.sgd.rs1s, time.sgd.rs1s, bias.sgd.rs1s) 
  
  gc(reset=TRUE)
  
  #----------------------------------------------------------------------------
  # Summarize and print the result of each replication
  #----------------------------------------------------------------------------
  results = matrix(NA, 5, 2)
  colnames(results) = c("Time", "Bias")
  rownames(results) = c("qr", "sqr.plug", "sqr.boot", "sgd.boot", "sgd.rs1s")
  results[,1] = c(time.qr, time.sqr.plug, time.sqr.boot, time.sgd.boot, time.sgd.rs1s)
  results[,2] = c(bias.qr, bias.sqr.plug, bias.sqr.boot, bias.sgd.boot, bias.sgd.rs1s)
  
  print(results)
  cat('\n \n')  
  
  toc()
} 
sink()

#total.mem = mem_used()
total.mem = NA

save(out.qr, out.sqr.plug, out.sqr.boot, out.sgd.boot, out.sgd.rs, out.sgd.rs1s, 
     n, burn, eff.n, replications, total.mem,
     file=paste0(filename,".RData"))
