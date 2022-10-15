semmcci
================
Ivan Jacob Agaloos Pesigan
2022-10-15

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
#> cp       0.2333 0.0263 20000 0.1490 0.1655 0.1813 0.2846 0.3002 0.3183
#> b        0.5082 0.0273 20000 0.4205 0.4397 0.4549 0.5616 0.5779 0.6004
#> a        0.4820 0.0264 20000 0.3959 0.4140 0.4299 0.5337 0.5495 0.5642
#> Y~~Y     0.5462 0.0244 20000 0.4660 0.4832 0.4979 0.5944 0.6095 0.6263
#> M~~M     0.7527 0.0339 20000 0.6471 0.6660 0.6859 0.8181 0.8402 0.8613
#> indirect 0.2449 0.0187 20000 0.1878 0.1995 0.2091 0.2829 0.2958 0.3085
#> direct   0.2333 0.0263 20000 0.1490 0.1655 0.1813 0.2846 0.3002 0.3183
#> total    0.4782 0.0265 20000 0.3930 0.4100 0.4262 0.5302 0.5463 0.5652
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
#> cp       0.2422 0.0268 20000 0.1553 0.1726 0.1899 0.2935 0.3107 0.3309
#> b        0.5123 0.0245 20000 0.4271 0.4474 0.4642 0.5595 0.5738 0.5920
#> a        0.4963 0.0239 20000 0.4093 0.4315 0.4480 0.5413 0.5546 0.5707
#> Y~~Y     0.5558 0.0234 20000 0.4798 0.4945 0.5094 0.6009 0.6154 0.6321
#> M~~M     0.7537 0.0236 20000 0.6743 0.6924 0.7070 0.7993 0.8138 0.8325
#> X~~X     1.0000 0.0000 20000 1.0000 1.0000 1.0000 1.0000 1.0000 1.0000
#> indirect 0.2542 0.0174 20000 0.1982 0.2101 0.2205 0.2882 0.2996 0.3120
#> direct   0.2422 0.0268 20000 0.1553 0.1726 0.1899 0.2935 0.3107 0.3309
#> total    0.4964 0.0239 20000 0.4168 0.4327 0.4480 0.5413 0.5556 0.5715
```

## Documentation

See [GitHub Pages](https://jeksterslab.github.io/semmcci/index.html) for
package documentation.
