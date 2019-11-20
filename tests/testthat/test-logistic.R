context('logistic')

test_that("logistic works", {
  X = matrix(rnorm(100,sd = 10),100,1)
  X1 = matrix(rnorm(1000),100,10)

  y = rbinom(100,1, 0.3)
  y_wrong = sample(1:9,100, replace = T)

  tol = 1e-5

  expect_true(abs(as.vector(glm(y~X, family= 'binomial')$coefficients) - as.vector(logistic(X,y)$coeffs)) < tol)
  expect_true(abs(as.vector(glm(y~X1, family= 'binomial')$coefficients) - as.vector(logistic(X1,y)$coeffs)) < tol)

  expect_error(logistic(X,y_wrong), 'Wrong type of data')
})
