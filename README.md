semmcci
================
Ivan Jacob Agaloos Pesigan
2021-03-21

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
#> y~x  0.2219 0.0346 20000  0.1037 0.1325 0.1538  0.2900  0.3110   0.3409
#> b    0.4835 0.0316 20000  0.3766 0.4020 0.4213  0.5452  0.5653   0.5835
#> a    0.4378 0.0316 20000  0.3345 0.3561 0.3753  0.5000  0.5189   0.5434
#> y~~y 1.0253 0.0460 20000  0.8755 0.9090 0.9349  1.1150  1.1413   1.1713
#> m~~m 1.0078 0.0453 20000  0.8657 0.8912 0.9202  1.0966  1.1263   1.1576
#> x~~x 1.0017     NA    NA      NA     NA     NA      NA      NA       NA
#> ab   0.2116 0.0208 20000  0.1494 0.1619 0.1726  0.2531  0.2672   0.2830
```

## More Information

See [GitHub Pages](https://jeksterslab.github.io/semmcci/index.html) for
package documentation.
