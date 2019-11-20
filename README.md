
<!-- README.md is generated from README.Rmd. Please edit that file -->
some.models
===========

<!-- badges: start -->
<!-- badges: end -->
The goal of some.models is to allow the user to use and understand both linear regression and logistic regression in a user friendly way. In this package, we create functions with output that is limited to metrics that are important when beginning to learn about these regression techniques. Compared to lm and glm,. these functions are easy to understand and interpret, and allow for easy, real-world, prediction capabilites.

Installation
------------

You can install the released version of some.models from GitHub. First, you should run

``` r
.rs.restartR()
```

followed by

``` r
install.packages('devtools')
devtools::install_github('benbren/some.models')
library("some.models")
```

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
library(some.models)

data(swiss)
X = swiss$Catholic
y = swiss$Infant.Mortality

fit = linear(X,y)

names(fit) # you can now choose what to do with all these things! 
#> [1] "coeffs"        "fitted_values" "se"            "res"          
#> [5] "predicted"     "t"             "conclusions"

ucla =  read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")

X = cbind(rep(1,dim(ucla)[1]), ucla$gpa)

y = ucla$admit

fit_logistic = logistic(X,y)
#> Converged at iteration 4

names(fit_logistic) # more things to choose from
#> [1] "coeffs"    "se"        "wald"      "p"         "or"        "fitted"   
#> [7] "predicted"
```

These are basic examples, please use

``` r
vignette('simple-examples')
```

to see more complex examples and how to use these functions in a more complete way.