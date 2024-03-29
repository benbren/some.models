#' Linear Regression and Prediction
#'
#' @param X
#' n by p design matrix of numeric covariates where n is the sample size and p is the parameter space.
#' @param y
#' n by 1 vector of numeric continuous outcomes
#' @param add_intercept
#' do you need to add an intercept column to your design matrix? we assume TRUE
#'
#' @param to_predict optional observation(s) for which you would like the outcomes predicted
#'
#'@return coefficients, fitted values, residuals, predictions (if you asked for them),
#'t- statistics and conclusions to the hypothesis test that the coefficient is 0.
#'
#' @examples
#'
#' data(mtcars)
#' y = mtcars$mpg
#' X = mtcars$cyl
#' # can also make X a matrix, no problem
#' fit = linear(X,y) # this assumes your design matrix does not have an intercept in it
#' betas = fit$coeffs # these are your OLS regression coefficients
#'
#' @export

linear = function(X, y, add_intercept = T , to_predict = NULL){

  if(is.null(dim(X))){ # a vector is X

    n = length(X)

  } else { # X is a matrix

    n = dim(X)[1]

  }

  if(add_intercept){ # this part adds the intercept if that was requested

    X = cbind(rep(1,n),X)

  }

  p = dim(X)[2]

  if(n != length(y)){

    if(n < length(y)){

      stop('You are missing some rows in your design matrix') # n < y

    } else {

      stop('You need more outcomes') # n > y
    }

  }
  if(n <= p){

    stop('Too many covariates for simple OLS') # overdetermined system, penalized regression is better

  }
  if(det(t(X)%*%X) == 0){ # this means X'X is not invertible (i.e it is not full column rank)

    stop('X must be full column-rank')

  }

  # all the simple matrix calculations of OLS

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

  ### PREDICTION #########

  if(!is.null(to_predict)){  # if to_predict is anything

   if(!is.null(dim(to_predict))){ # if to_predict is a matrix

      if(dim(to_predict)[2] == p){ # and a matrix with the intercept added in already

        warning('Predictions will be wrong if rows of your prediction data is not in the same order as your design')

        predicted =  to_predict %*% betas

      } else if(dim(to_predict)[2] == p-1){ # or if it is a matrix in need of the intercept

        warning('Predictions will be wrong if rows of your prediction data is not in the same order as your design - adding intercept')

        to_predict = cbind(1,to_predict)

        predicted =  to_predict %*% betas
      }

      else { # it is literally anything but a matrix with p or p- 1 columns

        stop('Design matrix to be predicted does not have the correct dimensions')
      }

    } else { # same as above, but in the case where the prediction is only one observation

      if(length(to_predict) == p){

        warning('Predictions will be wrong if rows of your prediction data is not in the same order as your design')

        predicted =  to_predict %*% betas

      } else if(length(to_predict) == p-1){

        warning('Predictions will be wrong if rows of your prediction data is not in the same order as your design - adding intercept')

        to_predict = c(1,to_predict)

        predicted =  to_predict %*% betas
      }

      else {

        stop('Design matrix to be predicted does not have the correct dimensions')
      }

    }
  }  else {

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

  return(rtrn) # cool!
}
