#' A function to do simple linear regression - the first of my first package - how cool!
#' 
#' This function will estimate the OLS estmates of beta, as well as the SD, t statistics, and hypothesis conclusions 
#' @param X an n x p design matrix of numeric covariates (non-missing) 
#' where n is the sample size and p is the parameter space. 
#' @param y and n x 1 vector of numeric continuous outcomes (also non-missing) 
#' @param  need_intercept do you need us to add an intercept column to your design matrix? we assume TRUE 
#' @export 
#' @examples 


linear = function(y,X, need_intercept = T){
  
  if(is.null(dim(X))){
    n = length(X)
    p = 1
  } else{ 
    n = dim(X)[1]
    p = dim(X)[2]
  } 
  
  if(n != length(y)){
    if(n < length(y)){
      stop('You are missing some rows in your design matrix')
    } else {
      stop('You need less outcomes')
    }
  }
  
  if(n <= p){
    stop('Too many covariates for simple OLS, bro')
  }
  
  if(need_intercept){
 
    X = cbind(rep(1,n),X)
  }
  
  betas = solve(t(X)%*%X) %*% (t(X)%*%y) 
  fitted = X %*% betas 
  residuals = fitted - y
  mse = (n-p-1)^-1 * sum(residuals^2)
  var_cov = mse * solve(t(X)%*%X)
  beta_var = diag(var_cov)
  
  t_stats = betas/sqrt(beta_var) 
  p = dt(t_stats,n-p-1)
  crit_value = qt(0.975, n-p)
  conclusion = ifelse( abs(t_stats) > crit_value, 'Reject', 'Fail to Reject' )
  
  rtrn = list(coeffs = betas, 
              fitted_values = fitted, 
              se = sqrt(beta_var),
              res = residuals, 
              varcov = var_cov, 
              t = t_stats, 
              p = p,
              conclusions = conclusion
              )
  
  return(rtrn)
  
}
