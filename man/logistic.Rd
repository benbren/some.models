% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logistic.R
\name{logistic}
\alias{logistic}
\title{Logistic Regression and Prediction}
\usage{
logistic(
  X,
  y,
  n = 1,
  i_max = 100,
  tol = 1e-04,
  to_predict = NULL,
  add_intercept = T
)
}
\arguments{
\item{X}{a design matrix - no restrictions, but it should have an intercept column or the results will
be wrong.}

\item{y}{an outcome vector. this should either be 1/0 or it should be the number of success out of corresponding n trials (below)}

\item{n}{the number of trials ^^. if y is 1/0, this should remain 1 and there is only one trial (either 1 or 0)
initialized at 1, as most data will come as 1/0.}

\item{i_max}{as generalized linear models use an iterative algorithm to estimate the parameter,
this is the number of iterations of IRWLS that you want to perform.}

\item{tol}{the tolerance to hop out of the algorithm.}

\item{to_predict}{a n optional matrix to have predictions made for - should be same dimensions as X, including the intercept,
or there will be an error/wrong inference.}

\item{add_intercept}{TRUE if your design matrix needs an intercept}
}
\value{
coefficients, standard errors, wald statistics, p-values, odds ratios and the fitted probabilities.
also returns and optional predictions for a set of test data.
}
\description{
Logistic Regression and Prediction
}
\examples{
y = rbinom(100,size = 1,prob = 0.4) # see vignette for n != 1 example
X = matrix(rnorm(1000,mean = 0, sd = 10),100,10)
fit = logistic(X,y)
odds_ratios = fit$or

}
