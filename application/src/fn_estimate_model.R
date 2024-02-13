
estimate_model <- function(data, qt, method, gamma_0, studentize, path){
  n_data = nrow(data)
  p_data = ncol(data)
  dep = data[,2]   #LWAGE of the data set

  bt.hat = NULL
  Sigma.hat = NULL
  ci = NULL
  
  #reg = cbind(data[,c(3:p_data)] )
  p = p_data -2 + 1 # intercept term will be added later
  
  
  #--------------------------------------------------
  # Quantile estimation with "nid"
  #--------------------------------------------------
  if (method == "qr"){
    time.start = Sys.time()
    
    fit.nid = rq(dep~data[,c(3:p_data)], tau = qt, method="pfn")
    if (p < 1000){
      #sum.fit.nid = summary(fit.nid, se="nid", covariance=TRUE)
      sum.fit.nid = summary(fit.nid, se="boot", covariance=TRUE)
    } else {
      sum.fit.nid = summary(fit.nid, se="boot", R=1000, covariance=TRUE)
    }
    #sum.fit.nid = NULL
    
    time.end = Sys.time()
    runtime = as.numeric(difftime(time.end, time.start, units = "secs"))
    
    bt.hat = coef(fit.nid)
    Sigma.hat = sum.fit.nid$cov
    time = runtime
  }
  #--------------------------------------------------
  # Quantile estimation with "boot"
  #--------------------------------------------------
  if (method == "boot"){
    time.start = proc.time()[3]
    
    fit.boot = rq(LWAGE~., tau = qt, data=data)
    sum.fit.boot = summary(fit.boot, se="boot", R=1000)
    
    time.end = proc.time()[3]
    runtime = time.end - time.start
    
    bt.hat = coef(fit.boot)
    Sigma.hat = sum.fit.boot$cov
    time = runtime
  }
  #--------------------------------------------------
  # Quantile estimation with "sgd"
  #--------------------------------------------------
  if (method == "SGDInference"){
    start.sgd = Sys.time()
    
    # Compute the starting value
    if (p > 1000){
      n_s = floor(0.10*n_data)  
    } else {
      n_s = floor(0.01*n_data)
    }
    
    
    error_occured = T
    while (error_occured){
      tryCatch(
        {
          # If conquer cannot compute the initial value well, we resample it. 
          subsample_index = sample(n_data, n_s)
          tic("conquer for bt_start")
          bt_start = conquer(data[subsample_index,c(3:p_data)], dep[subsample_index], tau = qt)$coeff
          toc()
          error_occured = F
        },
        error = function(e){
          print(e)
        }
      )
    }
    
    # Print parameters
    cat('gamma_0 = ', gamma_0, '\n')
    cat('studentize = ', studentize, '\n')
    
    # SGD estimation
    tic("SGD estimation Time : ")
    fit.sgd = sgdi_qr(dep~data[,c(3:p_data)], gamma_0 = gamma_0, bt_start = bt_start, alpha=0.501, qt= qt, inference = "rss", rss_idx = c(2,3), studentize=studentize, path=path)
    toc()
    end.sgd = Sys.time()
    runtime = as.numeric(difftime(end.sgd, start.sgd, units = "secs"))
    
    #bt.hat = fit.sgd$beta
    bt.hat = fit.sgd$coefficients
    Sigma.hat = fit.sgd$V
    if (path){
      cat("path = ", path,'\n')
      bt.hat.path = fit.sgd$beta_hat_path 
    }
    time = runtime
  }
  
  
  
  
  #--------------------------------------------------
  # Quantile estimation with "conquer1: asympt"
  #--------------------------------------------------
  if (method == "con1"){
    time.start = proc.time()[3]
    y = data$LWAGE
    x = as.matrix(data[,c(3:p_data)])
    #fit.con1 = conquer(x,y, tau = qt, ci="asymptotic")
    fit.con1 = conquer(x,y, tau = qt)
    
    time.end = proc.time()[3]
    runtime = time.end - time.start
    
    bt.hat = fit.con1$coeff
    ci = fit.con1$asyCI
    time = runtime
  }
  
  #--------------------------------------------------
  # Quantile estimation with "conquer2: bootstrap"
  #--------------------------------------------------
  if (method == "con2"){
    time.start = proc.time()[3]
    y = data$LWAGE
    x = as.matrix(data[,c(2:ncol(data))])
    fit.con = conquer(x,y, tau = qt, ci="bootstrap", B=10)
    
    time.end = proc.time()[3]
    runtime = time.end - time.start
    
    bt.hat = fit.con$coeff
    ci = fit.con$perCI
    time = runtime
  }
  
  
  
  # Return the output
  if (path){
    return(list(method=method, bt.hat=bt.hat, Sigma.hat=Sigma.hat, time=time, ci=ci, p=p, bt.hat.path=bt.hat.path)) 
  } else {
    return(list(method=method, bt.hat=bt.hat, Sigma.hat=Sigma.hat, time=time, ci=ci, p=p)) 
  }
}




