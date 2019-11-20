context('linear')

test_that("i get the correct coefficients for linear", {
  X = matrix(rnorm(100,sd = 10),100,1)
  X1 = matrix(rnorm(1000),100,10)
  X_wrong = matrix(rnorm(100),100,10)
  y = rnorm(100, sd = 4)
  y_wrong = rnorm(99, sd = 4)


  expect_equal(as.vector(lm(y ~ X)$coefficients), as.vector(linear(X,y)$coeffs))
  expect_equal(as.vector(lm(y ~ X1)$coefficients) , as.vector(linear(X1,y)$coeffs))

  expect_equal(as.vector(lm(y ~ X)$fitted.values), as.vector(linear(X,y)$fitted_values))
  expect_equal(as.vector(lm(y ~ X1)$fitted.values), as.vector(linear(X1,y)$fitted_values))

  expect_equal(abs(as.vector(lm(y ~ X)$residuals)), abs(as.vector(linear(X,y)$res)))
  expect_equal(abs(as.vector(lm(y ~ X1)$residuals)), abs(as.vector(linear(X1,y)$res)))

  expect_error(linear(X,y_wrong),'You need more outcomes')
  expect_error(linear(X_wrong,y), 'X must be full column-rank')
})
