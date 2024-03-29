
<!-- README.md is generated from README.Rmd. Please edit that file -->
some.models
===========

<!-- badges: start -->
[![Build Status](https://travis-ci.org/benbren/some.models.svg?branch=master)](https://travis-ci.org/benbren/some.models) [![Codecov test coverage](https://codecov.io/gh/benbren/some.models/branch/master/graph/badge.svg)](https://codecov.io/gh/benbren/some.models?branch=master) <!-- badges: end -->

The goal of some.models is to allow the user to use and understand both linear regression and logistic regression in a beginner friendly way. In this package, we create functions with output that is limited to metrics that are important when beginning to learn about these regression techniques. Compared to lm and glm, these functions are easy to understand and interpret, and allow for easy, real-world, prediction capabilities. I have found that most basic students of statistics are overwhelmed by the output of both lm and glm, and are unsure of how to interpret them or use them - or even how to extract the needed metrics from them! My hope for this package is for students, with knowledge of how to put data into these functions and formats, are able to better access and understand the functionalities of these methods.

Installation
------------

Okay, enough with the rant. You can install the released version of some.models from GitHub. First, you should run in your console (or in a script, whatever..)

``` r
.rs.restartR()
remove.packages('some.models')
```

just to be sure and to avoid any possible bugs!

Then you can run

``` r
install.packages('devtools')
devtools::install_github('benbren/some.models', build_vignettes = T)
library("some.models")
```

to get the package on your computer This will install two packages (bench and ggbeeswarm) automatically so that the user can access the vignettes!

What type of data should I use?
-------------------------------

### linear

You *need* to input an *n* × *p* design matrix *X* of numeric covariates, where one row is one subjects set of *p* covariates, and an *n* × 1 vector, *y* , of continuous outcomes. You have the option to have the intercept term added to your design matrix, and to provide the function with another *n* × *p* matrix of covariates (in the same order as your original *X* input) whose outcomes you would like the model to predict. This matrix can either be the same dimensions as your original input or the same dimension plus a column for the intercept.

### logistic

You *need* an *n* × *p* design matrix X of numeric covariates and either 1. an *n* × 1 binary outcome vector *y* or 2. two discrete *n* × 1 vectors, *y* and *n*, of "successes" (*y*) per "trials" (*n*). So, *y* &lt; *n*. The other options are optional, but as above specify the addition of an intercept and the possibility to predict outcomes using the model you have fit. There is also an option to change the number of iterations the algorithm runs to estimate the parameters, but this would rarely be needed.

Example
-------

This is a basic example which shows you how to solve a common problem and illustrates very basically the usage of both functions in the package:

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

X = cbind(ucla$gpa)

y = ucla$admit

fit_logistic = logistic(X,y)
#> Converged at iteration 4

names(fit_logistic) # more things to choose from
#> [1] "coeffs"    "se"        "wald"      "p"         "or"        "fitted"   
#> [7] "predicted"
```

These are basic examples which preclude any or planned analysis, please use

``` r
?linear
?logistic 
```

for more examples or, for even more info (recommended!!!!), use

``` r
browseVignettes(package = 'some.models')
```

and click HTML to see more complex examples and how to use these functions in a more complete way.

Have fun!!
----------
