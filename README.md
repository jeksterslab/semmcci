semmcci
================
Ivan Jacob Agaloos Pesigan
2021-03-20

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
#>         est     se    R ci_0.05 ci_0.5 ci_2.5 ci_97.5 ci_99.5 ci_99.95
#> y~x  0.2506 0.0353 2000  0.1266 0.1624 0.1817  0.3219  0.3462   0.3684
#> b    0.4977 0.0325 2000  0.4038 0.4171 0.4333  0.5611  0.5818   0.5966
#> a    0.5151 0.0303 2000  0.4157 0.4333 0.4528  0.5715  0.5906   0.6062
#> y~~y 1.0691 0.0476 2000  0.9269 0.9472 0.9745  1.1577  1.1944   1.2135
#> m~~m 1.0556 0.0468 2000  0.8936 0.9332 0.9691  1.1513  1.1722   1.1921
#> x~~x 1.0618     NA   NA      NA     NA     NA      NA      NA       NA
#> ab   0.2564 0.0225 2000  0.1817 0.1974 0.2128  0.3014  0.3160   0.3324
```

## More Information

See [GitHub Pages](https://jeksterslab.github.io/semmcci/index.html) for
package documentation.
