semmcci
================
Ivan Jacob Agaloos Pesigan
2021-03-19

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
#> y~x  0.2882 0.0359 2000  0.1690 0.1976 0.2163  0.3597  0.3762   0.3981
#> b    0.5333 0.0321 2000  0.4271 0.4478 0.4688  0.5951  0.6126   0.6278
#> a    0.5204 0.0323 2000  0.4172 0.4385 0.4580  0.5811  0.6021   0.6407
#> y~~y 0.9881 0.0431 2000  0.8546 0.8823 0.9048  1.0693  1.0941   1.1287
#> m~~m 0.9706 0.0437 2000  0.8203 0.8626 0.8809  1.0530  1.0747   1.0849
#> x~~x 0.9355     NA   NA      NA     NA     NA      NA      NA       NA
#> ab   0.2775 0.0240 2000  0.2078 0.2185 0.2320  0.3241  0.3435   0.3686
```

## More Information

See [GitHub Pages](https://jeksterslab.github.io/semmcci/index.html) for
package documentation.
