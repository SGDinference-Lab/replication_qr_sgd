sgdi_boot_qr = function(x, y, gamma_0=1, alpha=0.667, burn=1, inference="boot",
                        bt_start = NULL, path_output = NULL, qt=0.5,
                        studentize = TRUE, intercept = TRUE, n_boot=1000
){
  x = as.matrix(x)
  
  if (studentize){
    # Compute column means and standard errors and save them for later reconversion
    x_mean = apply(x, 2, mean)
    x_sd = apply(x, 2, sd)
    
    # Studentize each column if x
    x = apply(x, 2, function(.) (.-mean(.))/sd(.) )
  }
  
  # Attach a vector of 1's for an intercept term
  if (intercept){
    x = cbind(1, x)
  }
  
  # Get the dimension of x and the sample size: p and n
  p = ncol(as.matrix(x))
  n = length(y)
  
  # Initialize the bt_t, A_t, b_t, c_t
  if (is.null(bt_start)){
    bt_t = bar_bt_t = bt_start = matrix(0, nrow=p, ncol=1)
  } else {
    bt_t = bar_bt_t = matrix(bt_start, nrow=p, ncol=1)
  }
  A_t = matrix(0, p, p)
  b_t = matrix(0, p, 1)
  c_t = 0
  V_t = NULL
  
  n_path = length(path_output)
  cnt_path = 1
  beta_hat_path = matrix(NA, p, n_path)
  V_hat_path = array(NA, dim = c(p, p, n_path))
  
  #----------------------------------------------
  # Quantile Regression
  #----------------------------------------------
  
  out = sgdi_boot_qr_cpp(x, y, burn, gamma_0, alpha, bt_start=bt_t, "boot", tau=qt, n_boot=n_boot)
  beta_hat = out$beta_hat
  bar_coef_boot_mat = out$bar_coef_boot_mat
  
  
  
  # Re-scale parameters to reflect the studentization
  if (studentize){
    if (length(x_sd)>1){
      rescale_matrix = diag(1/x_sd)
    } else {
      rescale_matrix = 1/x_sd
    }
    if (intercept){
      # Redefine the rescale_matrix including the intercept term
      rescale_matrix = rbind(c(1,-(x_mean/x_sd)), cbind(0, rescale_matrix))
    }
    # Re-scale the parameters
    beta_hat = rescale_matrix %*% beta_hat
    bar_coef_boot_mat = rescale_matrix %*% bar_coef_boot_mat
  }
  
  
  
  if ( is.null(path_output)) {
    return(list(beta_hat=beta_hat, bar_coef_boot_mat = bar_coef_boot_mat))
  } else {
    return(list(beta_hat = beta_hat, V_hat = V_hat, beta_hat_path = beta_hat_path, V_hat_path = V_hat_path))
  }
  
}