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
#>         est     se     R ci_0.05 ci_0.5 ci_2.5 ci_97.5 ci_99.5 ci_99.95
#> y~x  0.2309 0.0353 20000  0.1151 0.1408 0.1617  0.3000  0.3227   0.3488
#> b    0.5211 0.0301 20000  0.4215 0.4427 0.4612  0.5802  0.5978   0.6190
#> a    0.5503 0.0325 20000  0.4428 0.4656 0.4860  0.6131  0.6361   0.6572
#> y~~y 0.9647 0.0433 20000  0.8261 0.8553 0.8807  1.0494  1.0755   1.1014
#> m~~m 1.0456 0.0470 20000  0.8970 0.9258 0.9533  1.1385  1.1665   1.1943
#> x~~x 0.9940     NA    NA      NA     NA     NA      NA      NA       NA
#> ab   0.2868 0.0239 20000  0.2127 0.2266 0.2404  0.3352  0.3516   0.3691
```

## More Information

See [GitHub Pages](https://jeksterslab.github.io/semmcci/index.html) for
package documentation.
