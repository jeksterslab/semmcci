semmcci
================
Ivan Jacob Agaloos Pesigan
2022-12-22

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![CRAN
Status](https://www.r-pkg.org/badges/version/semmcci)](https://cran.r-project.org/package=semmcci)
[![R-Universe
Status](https://jeksterslab.r-universe.dev/badges/semmcci)](https://jeksterslab.r-universe.dev)
[![R-CMD-check](https://github.com/jeksterslab/semmcci/workflows/R-CMD-check/badge.svg)](https://github.com/jeksterslab/semmcci/actions)
[![test-coverage](https://github.com/jeksterslab/semmcci/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/jeksterslab/semmcci/actions/workflows/test-coverage.yaml)
[![lint](https://github.com/jeksterslab/semmcci/actions/workflows/lint.yaml/badge.svg)](https://github.com/jeksterslab/semmcci/actions/workflows/lint.yaml)
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
a <- 0.50
b <- 0.50
cp <- 0.25
s2_em <- 1 - a^2
s2_ey <- 1 - cp^2 - a^2 * b^2 - b^2 * s2_em - 2 * cp * a * b
em <- rnorm(n = n, mean = 0, sd = sqrt(s2_em))
ey <- rnorm(n = n, mean = 0, sd = sqrt(s2_ey))
X <- rnorm(n = n)
M <- a * X + em
Y <- cp * X + b * M + ey
df <- data.frame(X, M, Y)
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
fit <- sem(data = df, model = model)
```

### Monte Carlo Confidence Intervals

The `fit` `lavaan` object can then be passed to the `MC()` function to
generate Monte Carlo confidence intervals.

``` r
MC(fit, R = 20000L, alpha = c(0.001, 0.01, 0.05))
#> Monte Carlo Confidence Intervals
#>             est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%
#> cp       0.2333 0.0264 20000 0.1465 0.1651 0.1817 0.2846 0.3012 0.3165
#> b        0.5082 0.0272 20000 0.4161 0.4385 0.4551 0.5611 0.5786 0.5988
#> a        0.4820 0.0264 20000 0.3997 0.4144 0.4302 0.5340 0.5499 0.5680
#> Y~~Y     0.5462 0.0244 20000 0.4660 0.4832 0.4979 0.5944 0.6095 0.6263
#> M~~M     0.7527 0.0339 20000 0.6434 0.6667 0.6867 0.8196 0.8399 0.8612
#> indirect 0.2449 0.0187 20000 0.1881 0.1983 0.2093 0.2825 0.2952 0.3081
#> direct   0.2333 0.0264 20000 0.1465 0.1651 0.1817 0.2846 0.3012 0.3165
#> total    0.4782 0.0267 20000 0.3920 0.4091 0.4257 0.5303 0.5459 0.5647
```

### Standardized Monte Carlo Confidence Intervals

Standardized Monte Carlo Confidence intervals can be generated by
passing the result of the `MC()` function to `MCStd()`.

> **Note:** We recommend setting `fixed.x = FALSE` when generating
> standardized estimates and confidence intervals to model the variances
> and covariances of the predictors if they are assumed to be random.

``` r
fit <- sem(data = df, model = model, fixed.x = FALSE)
unstd <- MC(fit, R = 20000L, alpha = c(0.001, 0.01, 0.05))
```

``` r
MCStd(unstd)
#> Standardized Monte Carlo Confidence Intervals
#>             est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%
#> cp       0.2422 0.0267 20000 0.1513 0.1722 0.1894 0.2939 0.3111 0.3284
#> b        0.5123 0.0247 20000 0.4319 0.4488 0.4636 0.5604 0.5764 0.5932
#> a        0.4963 0.0240 20000 0.4144 0.4323 0.4478 0.5414 0.5557 0.5709
#> Y~~Y     0.5558 0.0236 20000 0.4785 0.4948 0.5087 0.6017 0.6156 0.6320
#> M~~M     0.7537 0.0237 20000 0.6741 0.6913 0.7069 0.7995 0.8132 0.8282
#> X~~X     1.0000 0.0000 20000 1.0000 1.0000 1.0000 1.0000 1.0000 1.0000
#> indirect 0.2542 0.0176 20000 0.1967 0.2093 0.2200 0.2890 0.3008 0.3123
#> direct   0.2422 0.0267 20000 0.1513 0.1722 0.1894 0.2939 0.3111 0.3284
#> total    0.4964 0.0240 20000 0.4177 0.4327 0.4478 0.5417 0.5566 0.5738
```

## Documentation

See [GitHub Pages](https://jeksterslab.github.io/semmcci/index.html) for
package documentation.
