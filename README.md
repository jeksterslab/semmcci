semmcci
================
Ivan Jacob Agaloos Pesigan
2022-09-28

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![CRAN
Status](https://www.r-pkg.org/badges/version/semmcci)](https://cran.r-project.org/package=semmcci)
[![R-Universe
Status](https://jeksterslab.r-universe.dev/badges/semmcci)](https://jeksterslab.r-universe.dev)
[![R-CMD-check](https://github.com/jeksterslab/semmcci/workflows/R-CMD-check/badge.svg)](https://github.com/jeksterslab/semmcci/actions)
[![test-coverage](https://github.com/jeksterslab/semmcci/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/jeksterslab/semmcci/actions/workflows/test-coverage.yaml)
[![codecov](https://codecov.io/gh/jeksterslab/semmcci/branch/main/graph/badge.svg?token=KVLUET3DJ6)](https://codecov.io/gh/jeksterslab/semmcci)
<!-- badges: end -->

## Installation

You can install the CRAN release of `semmcci` with:

``` r
install.packages("semmcci")
```

You can install the development version of `semmcci` from
[GitHub](https://github.com/jeksterslab/semmcci) with:

``` r
if (!require("remotes")) install.packages("remotes")
remotes::install_github("jeksterslab/semmcci")
```

## Documentation

See [GitHub Pages](https://jeksterslab.github.io/semmcci/index.html) for
package documentation.

## Description

In the Monte Carlo method, a sampling distribution of parameter
estimates is generated from the multivariate normal distribution using
the parameter estimates and the sampling variance-covariance matrix.
Confidence intervals for defined parameters are generated by obtaining
percentiles corresponding to 100(1 - α)% from the generated sampling
distribution, where α is the significance level.

Monte Carlo confidence intervals for free and defined parameters in
models fitted in the structural equation modeling package `lavaan` can
be generated using the `semmcci` package. The package has two main
functions, namely, `MC()` and `MCStd()`. The output of `lavaan` is
passed as the first argument to the `MC()` function to generate Monte
Carlo confidence intervals. Monte Carlo confidence intervals for the
standardized estimates can also be generated by passing the output of
the `MC()` function to the `MCStd()` function.

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
#> cp       0.2373 0.0368 20000 0.1194 0.1426 0.1647 0.3091 0.3308 0.3561
#> b        0.4817 0.0334 20000 0.3734 0.3979 0.4165 0.5471 0.5667 0.5930
#> a        0.5098 0.0308 20000 0.4094 0.4305 0.4491 0.5702 0.5886 0.6057
#> Y~~Y     1.0581 0.0473 20000 0.9029 0.9361 0.9647 1.1517 1.1809 1.2134
#> M~~M     0.9712 0.0438 20000 0.8350 0.8594 0.8851 1.0556 1.0841 1.1115
#> indirect 0.2456 0.0225 20000 0.1777 0.1914 0.2029 0.2914 0.3071 0.3227
#> direct   0.2373 0.0368 20000 0.1194 0.1426 0.1647 0.3091 0.3308 0.3561
#> total    0.4829 0.0359 20000 0.3687 0.3915 0.4130 0.5528 0.5746 0.6026
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
```

``` r
MCStd(unstd)
#> Standardized Monte Carlo Confidence Intervals
#>             est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%
#> cp       0.1930 0.0294 20000 0.0958 0.1171 0.1355 0.2496 0.2682 0.2904
#> b        0.4340 0.0274 20000 0.3395 0.3615 0.3801 0.4868 0.5028 0.5217
#> a        0.4602 0.0249 20000 0.3699 0.3931 0.4098 0.5075 0.5209 0.5379
#> Y~~Y     0.6972 0.0242 20000 0.6152 0.6320 0.6482 0.7430 0.7571 0.7733
#> M~~M     0.7882 0.0229 20000 0.7107 0.7286 0.7424 0.8320 0.8455 0.8632
#> X~~X     1.0000 0.0000 20000 1.0000 1.0000 1.0000 1.0000 1.0000 1.0000
#> indirect 0.1998 0.0170 20000 0.1473 0.1570 0.1674 0.2335 0.2444 0.2578
#> direct   0.1930 0.0294 20000 0.0958 0.1171 0.1355 0.2496 0.2682 0.2904
#> total    0.3928 0.0268 20000 0.3011 0.3220 0.3389 0.4439 0.4602 0.4785
```
