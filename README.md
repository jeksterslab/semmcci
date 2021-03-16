semmcci
================
Ivan Jacob Agaloos Pesigan
2021-03-16

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R build
status](https://github.com/jeksterslab/semmcci/workflows/R-CMD-check/badge.svg?branch=master)](https://github.com/jeksterslab/semmcci/actions?workflow=R-CMD-check)
[![Travis build
status](https://travis-ci.com/jeksterslab/semmcci.svg?branch=master)](https://travis-ci.com/jeksterslab/semmcci)
[![codecov](https://codecov.io/github/jeksterslab/semmcci/branch/master/graphs/badge.svg)](https://codecov.io/github/jeksterslab/semmcci)
<!-- badges: end -->

## Monte Carlo Confidence Intervals

`semmcci` calculates Monte Carlo confidence intervals for parameters
defined using the `:=` operator in the structural equation modeling
package `lavaan`. The output of `lavaan` is passed as the first argument
to the `mc` function in `semmcci` to generate Monte Carlo confidence
intervals for defined parameter/s.

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
data <- data.frame(
  x,
  m,
  y
)
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
mc(fit)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

    #>       est     se     R ci_0.05 ci_0.5 ci_2.5 ci_97.5 ci_99.5 ci_99.95
    #> ab 0.2654 0.0234 20000  0.1901 0.2094 0.2213  0.3124  0.3281   0.3464

## More Information

See [GitHub Pages](https://jeksterslab.github.io/semmcci/index.html) for
package documentation.
