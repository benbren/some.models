context('logistic')

test_that("logistic works", {
  X = matrix(rnorm(100,sd = 10),100,1)
  X1 = matrix(rnorm(1000),100,10)

  y = rbinom(100,1, 0.3)
  y_wrong = sample(1:9,100, replace = T)

  tol = 1e-5

  expect_equal(as.vector(glm(y~X, family= 'binomial')$coefficients), as.vector(logistic(X,y)$coeffs))
  expect_equal(as.vector(glm(y~X1, family= 'binomial')$coefficients),  as.vector(logistic(X1,y)$coeffs))

  expect_error(logistic(X,y_wrong), 'Wrong type of data')
  expect_error(logistic(X,y, i_max = 1), "IRWLS failed to converge")
  expect_error(logistic(X,y,to_predict = c(12,1,3)),'Cant predict - incorrect # of covariates')
  expect_error(logistic(X,y,to_predict = matrix(rnorm(100*100),100,100)),'Cant predict - incorrect # of covariates')

  expect_warning(logistic(X,y, to_predict = 4), 'Prediction wrong if to_predict not in same order as design matrix - adding intercept')
  expect_warning(logistic(X,y, to_predict = c(1,4)), 'Prediction wrong if to_predict not in same order as design matrix')

  expect_warning(logistic(X1,y, to_predict = sample(1:10,10)), 'Prediction wrong if to_predict not in same order as design matrix - adding intercept')
  expect_warning(logistic(X,y, to_predict = c(1,4)), 'Prediction wrong if to_predict not in same order as design matrix')


})
