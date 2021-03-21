semmcci
================
Ivan Jacob Agaloos Pesigan
2021-03-22

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R build
status](https://github.com/jeksterslab/semmcci/workflows/R-CMD-check/badge.svg?branch=master)](https://github.com/jeksterslab/semmcci/actions?workflow=R-CMD-check)
[![Travis build
status](https://travis-ci.com/jeksterslab/semmcci.svg?branch=master)](https://travis-ci.com/jeksterslab/semmcci)
[![codecov](https://codecov.io/github/jeksterslab/semmcci/branch/master/graphs/badge.svg)](https://codecov.io/github/jeksterslab/semmcci)
<!-- badges: end -->

## Monte Carlo Confidence Intervals

`semmcci` calculates Monte Carlo confidence intervals. The output of the
structural equation modeling package `lavaan` is passed as the first
argument to the `mc` function in `semmcci` to generate Monte Carlo
confidence intervals.

## Installation

You can install the released version of `semmcci` from
[GitHub](https://github.com/jeksterslab/semmcci) with:

``` r
library(devtools)
install_github("jeksterslab/semmcci")
```

## Example

### Data

``` r
n <- 1000
x <- rnorm(n = n)
m <- 0.50 * x + rnorm(n = n)
y <- 0.25 * x + 0.50 * m + rnorm(n = n)
data <- data.frame(x, m, y)
```

### Model Specification in lavaan

The first step is to define a `lavaan` model using the `lavaan` model
syntax. In this example, we look at a simple mediation model where `x`
is the predictor, `m` is the mediator, and `y` is the dependent
variable. The indirect effect is defined by the product of the slopes of
paths `x` to `m` labeled as `a` and `m` to `y` labeled as `b`. We are
interested in the confidence intervals of `ab` defined as the product of
`a` and `b` using the `:=` operator.

``` r
model <- "
  y ~ x + b * m
  m ~ a * x
  ab := a * b
"
```

### Model Fitting in lavaan

We can now fit the model and save the `lavaan` object to `fit`.

``` r
library(lavaan)
#> This is lavaan 0.6-8
#> lavaan is FREE software! Please report any bugs.
fit <- sem(
  model = model,
  data = data
)
```

### Monte Carlo Method Confidence Intervals

We can now construct confidence intervals around `ab` by passing `fit`
as an argument to the `mc` function.

``` r
library(semmcci)
out <- mc(fit)
semmcci::print(out)
#> Monte Carlo Confidence Intervals
#>         est     se     R ci_0.05 ci_0.5 ci_2.5 ci_97.5 ci_99.5 ci_99.95
#> y~x  0.2393 0.0344 20000  0.1223 0.1500 0.1723  0.3068  0.3277   0.3494
#> b    0.4681 0.0304 20000  0.3623 0.3878 0.4084  0.5270  0.5455   0.5684
#> a    0.5094 0.0318 20000  0.4064 0.4276 0.4467  0.5709  0.5889   0.6078
#> y~~y 0.9503 0.0421 20000  0.8126 0.8401 0.8671  1.0333  1.0595   1.0881
#> m~~m 1.0432 0.0468 20000  0.8873 0.9254 0.9519  1.1354  1.1649   1.1954
#> x~~x 1.0242     NA    NA      NA     NA     NA      NA      NA       NA
#> ab   0.2384 0.0213 20000  0.1713 0.1843 0.1975  0.2811  0.2955   0.3089
```

## More Information

See [GitHub Pages](https://jeksterslab.github.io/semmcci/index.html) for
package documentation.
