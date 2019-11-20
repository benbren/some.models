#' A function to perfrom logistic regression on data with either binary outcomes (1/0)
#' or with data that indicates the number of successes out of the number of overall trials 
#' 
#' @param X a design matrix - no restrictions, but it should have an intercept column or the results will 
#' be wrong.  
#' @param y an outcome vector. this should either be 1/0 or it should be the number of success out of n trials 
#' @param n the number of trials ^^. if y is 1/0, this should remain 1 and there is only one trial (either 1 or 0)
#' intialized at 1, as most data will come as 1/0. 
#' @param i_max as generalized linear models use an iterative algorithm to estimate the parameter,
#' this is the number of iterations of IRWLS that you want to perform.
#' @param tol the tolerance to hop out of the algorithm. 
#' @param to_predict a n optional matrix to have predictions made for - should be same dimensions as X, including the intercept, 
#' or there will be an error/wrong inference. 
#' 
#' @return coefficients, standard errors, wald statistics, p-values, odds ratios and the fitted probabilites. 
#' also returns and optional predictions for a set of test data. 
#' @examples 
#' y = rbinom(100,size = 1,prob = 0.4) # see vignette for n != 1 example 
#' X = matrix(rnorm(1000,mean = 0, sd = 10),100,10)
#' fit = logistic(X,y)
#' odds_ratios = fit$or 
#' 
#' @export

logistic = function(X,y,n = 1, i_max = 100, tol = 1e-4, to_predict = NULL){

  q = dim(X)[2]

  # Initializing values for IRWLS #############

  betas = rep(0,q)
  W = NULL
  err = Inf 
  i = 1
  VCOV = NULL
  std_errs = NULL
  p_s = NULL

# IRWLS Algorithm #####
  
  while(err > tol & i < i_max){
  
    pis = exp(X%*%betas)/(1+exp(X%*%betas))
    
    mus = n*pis
    
    vs = mus*(1-pis)
  
    V = diag(as.vector(vs))
  
    Z = X%*%betas + solve(V)%*%(y - mus)
  
    betas_0 = betas
  
    VCOV = solve(t(X)%*%V%*%X)

    std_errs = sqrt(diag(VCOV))

    W = (betas/std_errs)**2

    betas = VCOV%*%(t(X)%*%V%*%Z)
  
    err = norm(betas-betas_0, type = '2')

    p_s = 1 - pchisq(W,1)
   
    i = i+1
  }

  if(i < i_max){
    
    message(paste("Converged at iteration",i-1))
    
  } else { stop("IRWLS failed to converge") }
  
  
  if(!is.null(to_predict)){
    
    predicted_pi = exp(to_predict%*%betas)/(1+ exp(to_predict%*%betas))

    predicted = ifelse(predicted_pi < 0.5, 0,1)
    
  } else {
    
    predicted = NULL
    
  }
  
  r = list(coeffs = betas, se = std_errs, wald = W, p = p_s, or = exp(betas), fitted = pis, predicted = predicted)
  
  return(r)
  
}


