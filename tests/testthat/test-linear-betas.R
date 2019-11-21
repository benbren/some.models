context('linear')

test_that("linear works", {

  X = matrix(rnorm(100,sd = 10),100,1)
  X1 = matrix(rnorm(1000),100,10)
  X_wrong = matrix(rnorm(100),100,10)
  y = rnorm(100, sd = 4)
  y_wrong = rnorm(99, sd = 4)


  # check if the coefficients are the same
  expect_equal(as.vector(lm(y ~ X)$coefficients), as.vector(linear(X,y)$coeffs))
  expect_equal(as.vector(lm(y ~ X1)$coefficients) , as.vector(linear(X1,y)$coeffs))

  # check if the fitted values are the same
  expect_equal(as.vector(lm(y ~ X)$fitted.values), as.vector(linear(X,y)$fitted_values))
  expect_equal(as.vector(lm(y ~ X1)$fitted.values), as.vector(linear(X1,y)$fitted_values))


  # check if the residuals are the same (up to the sign)
  expect_equal(abs(as.vector(lm(y ~ X)$residuals)), abs(as.vector(linear(X,y)$res)))
  expect_equal(abs(as.vector(lm(y ~ X1)$residuals)), abs(as.vector(linear(X1,y)$res)))

  expect_error(linear(X,y_wrong),'You need more outcomes') # y < n
  expect_error(linear(X[-1,],y),'You are missing some rows in your design matrix') # y > n
  expect_error(linear(t(X1),y[1:10]), 'Too many covariates for simple OLS') # p > n
  expect_error(linear(X_wrong,y), 'X must be full column-rank') # X has linearly dependent columns
  # below is if the prediction does not have the same dimension as X
  expect_error(linear(X1,y, to_predict = matrix(c(1,4,2,4),2,2)),
                                    "Design matrix to be predicted does not have the correct dimensions")
  expect_error(linear(X1,y, to_predict = c(1,4,2,4)),
               "Design matrix to be predicted does not have the correct dimensions")


  expect_warning(linear(X1,y,to_predict = cbind(1,matrix(sample(1:100, dim(X1)[2]*2),2, dim(X1)[2]))),
  'Predictions will be wrong if rows of your prediction data is not in the same order as your design')

  expect_warning(linear(X1,y,to_predict = matrix(sample(1:100, dim(X1)[2]*2), 2, dim(X1)[2])),
                 'Predictions will be wrong if rows of your prediction data is not in the same order as your design - adding intercept')


  expect_warning(linear(X1,y,to_predict = c(1,sample(1:100, dim(X1)[2]))),
                 'Predictions will be wrong if rows of your prediction data is not in the same order as your design')

  expect_warning(linear(X1,y,to_predict = sample(1:100, dim(X1)[2])),
                 'Predictions will be wrong if rows of your prediction data is not in the same order as your design - adding intercept')

})
