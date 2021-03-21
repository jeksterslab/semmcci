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
#> y~x  0.2719 0.0344 20000  0.1565 0.1847 0.2050  0.3390  0.3599   0.3824
#> b    0.4405 0.0304 20000  0.3468 0.3631 0.3807  0.4994  0.5183   0.5361
#> a    0.4933 0.0321 20000  0.3887 0.4097 0.4312  0.5565  0.5747   0.6003
#> y~~y 0.9593 0.0430 20000  0.8202 0.8506 0.8751  1.0438  1.0711   1.1083
#> m~~m 1.0397 0.0466 20000  0.8865 0.9212 0.9486  1.1307  1.1590   1.1934
#> x~~x 0.9967     NA    NA      NA     NA     NA      NA      NA       NA
#> ab   0.2173 0.0206 20000  0.1552 0.1666 0.1780  0.2594  0.2727   0.2898
```

## More Information

See [GitHub Pages](https://jeksterslab.github.io/semmcci/index.html) for
package documentation.
