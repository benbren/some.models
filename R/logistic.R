logistic = function(X,y,n = 1, i_max = 100, tol = 1e-4){

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
  
  r = list(coeffs = betas, se = std_errs, wald = W, p = p_s)
  
  return(r)
}


