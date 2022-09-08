semmcci
================
Ivan Jacob Agaloos Pesigan
2022-09-08

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R-CMD-check](https://github.com/jeksterslab/semmcci/workflows/R-CMD-check/badge.svg)](https://github.com/jeksterslab/semmcci/actions)
[![test-coverage](https://github.com/jeksterslab/semmcci/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/jeksterslab/semmcci/actions/workflows/test-coverage.yaml)
[![codecov](https://codecov.io/gh/jeksterslab/semmcci/branch/main/graph/badge.svg?token=KVLUET3DJ6)](https://codecov.io/gh/jeksterslab/semmcci)
[![pkgdown](https://github.com/jeksterslab/semmcci/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/jeksterslab/semmcci/actions/workflows/pkgdown.yaml)
<!-- badges: end -->

## Description

Monte Carlo confidence intervals for free and defined parameters in
models fitted in the structural equation modeling package `lavaan` can
be generated using the `semmcci` package. The package has two main
functions, namely, `MC()` and `MCStd()`. The output of `lavaan` is
passed as the first argument to the `MC()` function to generate Monte
Carlo confidence intervals. Monte Carlo confidence intervals for the
standardized estimates can also be generated by passing the output of
the `MC()` function to the `MCStd()` function.

## Installation

You can install the released version of `semmcci` from
[GitHub](https://github.com/jeksterslab/semmcci) with:

``` r
install.packages("remotes")
remotes::install_github("jeksterslab/semmcci")
```

## The Monte Carlo Method

In the Monte Carlo method, a sampling distribution of parameter
estimates is generated from the multivariate normal distribution using
the parameter estimates and the sampling variance-covariance matrix.
Confidence intervals for defined parameters are generated by obtaining
percentiles corresponding to 100(1 - α)% from the generated sampling
distribution, where α is the significance level.

## Example

A common application of the Monte Carlo method is to generate confidence
intervals for the indirect effect. In the simple mediation model,
variable `X` has an effect on variable `Y`, through a mediating variable
`M`. This mediating or indirect effect is a product of path coefficients
from the fitted model.

``` r
library(semmcci)
library(lavaan)
```

### Data

``` r
n <- 1000
X <- rnorm(n = n)
M <- 0.50 * X + rnorm(n = n)
Y <- 0.25 * X + 0.50 * M + rnorm(n = n)
data <- data.frame(X, M, Y)
```

### Model Specification

The indirect effect is defined by the product of the slopes of paths `X`
to `M` labeled as `a` and `M` to `Y` labeled as `b`. In this example, we
are interested in the confidence intervals of `indirect` defined as the
product of `a` and `b` using the `:=` operator in the `lavaan` model
syntax.

``` r
model <- "
  Y ~ cp * X + b * M
  M ~ a * X
  indirect := a * b
  direct := cp
  total := cp + (a * b)
"
```

### Model Fitting

We can now fit the model using the `sem()` function from `lavaan`.

``` r
fit <- sem(data = data, model = model)
```

### Monte Carlo Confidence Intervals

The `fit` `lavaan` object can then be passed to the `MC()` function to
generate Monte Carlo confidence intervals.

``` r
MC(fit, R = 20000L, alpha = c(0.001, 0.01, 0.05))
#> Monte Carlo Confidence Intervals
#>             est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%
#> cp       0.2875 0.0350 20000 0.1751 0.1969 0.2186 0.3565 0.3757 0.3986
#> b        0.5001 0.0320 20000 0.3974 0.4178 0.4378 0.5638 0.5826 0.6072
#> a        0.4843 0.0307 20000 0.3838 0.4068 0.4241 0.5444 0.5644 0.5866
#> Y~~Y     0.9738 0.0437 20000 0.8294 0.8638 0.8886 1.0610 1.0844 1.1139
#> M~~M     0.9524 0.0425 20000 0.8110 0.8423 0.8682 1.0351 1.0598 1.0944
#> X~~X     1.0128 0.0000 20000 1.0128 1.0128 1.0128 1.0128 1.0128 1.0128
#> indirect 0.2422 0.0219 20000 0.1788 0.1887 0.2005 0.2864 0.3017 0.3170
#> direct   0.2875 0.0350 20000 0.1751 0.1969 0.2186 0.3565 0.3757 0.3986
#> total    0.5296 0.0348 20000 0.4168 0.4400 0.4615 0.5975 0.6181 0.6405
```

### Standardized Monte Carlo Confidence Intervals

Standardized Monte Carlo Confidence intervals can be generated by
passing the result of the `MC()` function to `MCStd()`.

> **Note:** We recommend setting `fixed.x = FALSE` when generating
> standardized estimates and confidence intervals to model the variances
> and covariances of the predictors if they are assumed to be random.

``` r
fit <- sem(data = data, model = model, fixed.x = FALSE)
unstd <- MC(fit, R = 20000L, alpha = c(0.001, 0.01, 0.05))
MCStd(unstd)
#> Standardized Monte Carlo Confidence Intervals
#>             est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%
#> cp       0.2365 0.0281 20000 0.1374 0.1633 0.1812 0.2917 0.3097 0.3305
#> b        0.4460 0.0264 20000 0.3602 0.3764 0.3934 0.4971 0.5132 0.5334
#> a        0.4468 0.0252 20000 0.3603 0.3795 0.3963 0.4955 0.5101 0.5281
#> Y~~Y     0.6509 0.0244 20000 0.5708 0.5869 0.6017 0.6972 0.7117 0.7260
#> M~~M     0.8004 0.0225 20000 0.7211 0.7398 0.7545 0.8429 0.8560 0.8702
#> X~~X     1.0000 0.0000 20000 1.0000 1.0000 1.0000 1.0000 1.0000 1.0000
#> indirect 0.1993 0.0166 20000 0.1468 0.1583 0.1675 0.2329 0.2428 0.2543
#> direct   0.2365 0.0281 20000 0.1374 0.1633 0.1812 0.2917 0.3097 0.3305
#> total    0.4358 0.0257 20000 0.3457 0.3669 0.3849 0.4860 0.5010 0.5163
```

## More Information

See [GitHub Pages](https://jeksterslab.github.io/semmcci/index.html) for
package documentation.
