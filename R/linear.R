#' A function to do simple linear regression - the first of my first package - how cool!
#' This function will estimate the OLS estmates of beta, as well as the SD, t statistics, and hypothesis conclusions
#'
#' @param X
#' n by p design matrix of numeric covariates where n is the sample size and p is the parameter space.
#' @param y
#' n by 1 vector of numeric continuous outcomes
#' @param need_intercept
#' do you need to add an intercept column to your design matrix? we assume TRUE
#'
#' @examples
#'
#' data(mtcars)
#' y = mtcars$mpg
#' X = mtcars$cyl
#' # can also make X a matrix, no problem
#' fit = linear(X,y)
#' betas = fit$coeffs
#'
#' @export

linear = function(X, y, need_intercept = T , to_predict = NULL){



  if(is.null(dim(X))){

    n = length(X)


  } else {

    n = dim(X)[1]

  }


  if(need_intercept){

    X = cbind(rep(1,n),X)

  }

  p = dim(X)[2]


  if(n != length(y)){

    if(n < length(y)){

      stop('You are missing some rows in your design matrix')

    } else {

      stop('You need less outcomes')

    }

  }
  if(n <= p){

    stop('Too many covariates for simple OLS')

  }

  Xt_X_inv = solve(t(X)%*%X)
  betas = Xt_X_inv %*% (t(X)%*%y)
  fitted = X %*% betas
  residuals = fitted - y
  mse = sum(residuals^2)/(n-p-1)
  var_cov = mse * Xt_X_inv
  beta_var = diag(var_cov)
  t_stats = betas/sqrt(beta_var)
  crit_value = qt(0.975, n-p-1)
  conclusion = ifelse( abs(t_stats) > crit_value, 'Reject', 'Fail to Reject')


  if(!is.null(to_predict)){


    if(all(dim(to_predict) == c(n,p))){

      warning('Predictions will be wrong if your prediction data is not in the same order as your design')

      predicted =  to_predict %*% betas

    } else if (all(dim(to_predict) == c(n,p-1))){

      warning('Predictions will be wrong if your prediction data is not in the same order as your design')

      warning('Adding intercept to prediction')

      to_predict = cbind(rep(1,n),to_predict)

      predicted =  to_predict %*% betas
    }

    else (

      stop('Design matrix to be predicted does not have the correct dimensions')

    )

  } else {

    predicted = NULL

  }

  rtrn = list(coeffs = betas,

              fitted_values = fitted,

              se = sqrt(beta_var),

              res = residuals,

              predicted = predicted,

              t = t_stats,

              conclusions = conclusion
              )

  return(rtrn)
}
