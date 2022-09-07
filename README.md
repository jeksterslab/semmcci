semmcci
================
Ivan Jacob Agaloos Pesigan
2022-09-07

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R-CMD-check](https://github.com/jeksterslab/semmcci/workflows/R-CMD-check/badge.svg)](https://github.com/jeksterslab/semmcci/actions)
[![codecov](https://codecov.io/gh/jeksterslab/semmcci/branch/main/graph/badge.svg)](https://codecov.io/gh/jeksterslab/semmcci)
<!-- badges: end -->

# semmcci: Monte Carlo Confidence Intervals in Structural Equation Modeling

## Description

`semmcci` calculates Monte Carlo confidence intervals for free and
defined parameters in the structural equation modeling package `lavaan`.
The output of `lavaan` is passed as the first argument to the `MC()`
function to generate Monte Carlo confidence intervals. Monte Carlo
confidence intervals for the standardized estimates can also be
generated by passing the output of the `MC()` function to the `MCStd()`
function.

## Installation

You can install the released version of `semmcci` from
[GitHub](https://github.com/jeksterslab/semmcci) with:

``` r
install.packages("remotes")
remotes::install_github("jeksterslab/semmcci")
```

## Example

This is an example of a simple mediation model where `X` is the
predictor, `M` is the mediator, and `Y` is the dependent variable.

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
are interested in the confidence intervals of `ab` defined as the
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

The `fit` `lavaan` object can then be passed to the `MC()` function from
`semmcci` to generate Monte Carlo confidence intervals.

``` r
MC(fit, R = 20000L, alpha = c(0.001, 0.01, 0.05))
#> Monte Carlo Confidence Intervals
#>             est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%
#> cp       0.2622 0.0378 20000 0.1437 0.1661 0.1885 0.3351 0.3587 0.3830
#> b        0.4478 0.0335 20000 0.3386 0.3609 0.3812 0.5126 0.5349 0.5586
#> a        0.4856 0.0318 20000 0.3822 0.4030 0.4223 0.5475 0.5663 0.5908
#> Y~~Y     1.1176 0.0497 20000 0.9480 0.9882 1.0192 1.2149 1.2453 1.2818
#> M~~M     0.9902 0.0445 20000 0.8462 0.8765 0.9034 1.0777 1.1050 1.1373
#> X~~X     0.9641     NA    NA     NA     NA     NA     NA     NA     NA
#> indirect 0.2174 0.0217 20000 0.1518 0.1642 0.1763 0.2611 0.2758 0.2950
#> direct   0.2622 0.0378 20000 0.1437 0.1661 0.1885 0.3351 0.3587 0.3830
#> total    0.4797 0.0370 20000 0.3647 0.3833 0.4062 0.5514 0.5734 0.6010
```

### Standardized Monte Carlo Confidence Intervals

Standardized Monte Carlo Confidence intervals can be generated by
passing the result of the `MC()` function to `MCStd()`.

> **Note:** We recommend setting `fixed.x = FALSE` when generating
> standardized estimates and confidence intervals to model the
> variance/s of the predictor/s.

``` r
fit <- sem(data = data, model = model, fixed.x = FALSE)
unstd <- MC(fit, R = 20000L, alpha = c(0.001, 0.01, 0.05))
MCStd(unstd)
#> Standardized Monte Carlo Confidence Intervals
#>             est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%
#> cp       0.2076 0.0295 20000 0.1076 0.1332 0.1495 0.2659 0.2837 0.3041
#> b        0.3984 0.0278 20000 0.3083 0.3253 0.3429 0.4522 0.4685 0.4884
#> a        0.4321 0.0260 20000 0.3465 0.3652 0.3805 0.4820 0.4990 0.5150
#> Y~~Y     0.7267 0.0240 20000 0.6475 0.6625 0.6778 0.7718 0.7855 0.8007
#> M~~M     0.8133 0.0224 20000 0.7347 0.7510 0.7677 0.8552 0.8666 0.8799
#> X~~X     1.0000 0.0000 20000 1.0000 1.0000 1.0000 1.0000 1.0000 1.0000
#> indirect 0.1722 0.0162 20000 0.1233 0.1322 0.1409 0.2048 0.2152 0.2286
#> direct   0.2076 0.0295 20000 0.1076 0.1332 0.1495 0.2659 0.2837 0.3041
#> total    0.3798 0.0273 20000 0.2919 0.3104 0.3260 0.4328 0.4485 0.4639
```

## More Information

See [GitHub Pages](https://jeksterslab.github.io/semmcci/index.html) for
package documentation.
