semmcci
================
Ivan Jacob Agaloos Pesigan
2022-09-07

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R-CMD-check](https://github.com/jeksterslab/semmcci/workflows/R-CMD-check/badge.svg)](https://github.com/jeksterslab/semmcci/actions)
[![test-coverage](https://github.com/jeksterslab/semmcci/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/jeksterslab/semmcci/actions/workflows/test-coverage.yaml)
[![pkgdown](https://github.com/jeksterslab/semmcci/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/jeksterslab/semmcci/actions/workflows/pkgdown.yaml)
<!-- badges: end -->

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
#> cp       0.2811 0.0361 20000 0.1613 0.1880 0.2094 0.3513 0.3754 0.3985
#> b        0.4912 0.0307 20000 0.3903 0.4137 0.4314 0.5517 0.5713 0.5918
#> a        0.4693 0.0343 20000 0.3545 0.3797 0.4020 0.5361 0.5590 0.5849
#> Y~~Y     0.9933 0.0444 20000 0.8528 0.8785 0.9059 1.0804 1.1078 1.1375
#> M~~M     1.0672 0.0475 20000 0.9109 0.9442 0.9726 1.1591 1.1863 1.2137
#> X~~X     0.9040     NA    NA     NA     NA     NA     NA     NA     NA
#> indirect 0.2305 0.0223 20000 0.1611 0.1762 0.1888 0.2761 0.2907 0.3062
#> direct   0.2811 0.0361 20000 0.1613 0.1880 0.2094 0.3513 0.3754 0.3985
#> total    0.5117 0.0371 20000 0.3913 0.4143 0.4387 0.5844 0.6075 0.6290
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
#> cp       0.2192 0.0279 20000 0.1276 0.1467 0.1643 0.2738 0.2904 0.3109
#> b        0.4532 0.0260 20000 0.3610 0.3840 0.4015 0.5026 0.5183 0.5355
#> a        0.3965 0.0267 20000 0.3074 0.3259 0.3436 0.4486 0.4642 0.4813
#> Y~~Y     0.6678 0.0244 20000 0.5887 0.6034 0.6189 0.7146 0.7301 0.7483
#> M~~M     0.8428 0.0212 20000 0.7684 0.7846 0.7988 0.8819 0.8938 0.9055
#> X~~X     1.0000 0.0000 20000 1.0000 1.0000 1.0000 1.0000 1.0000 1.0000
#> indirect 0.1797 0.0162 20000 0.1294 0.1393 0.1484 0.2123 0.2236 0.2352
#> direct   0.2192 0.0279 20000 0.1276 0.1467 0.1643 0.2738 0.2904 0.3109
#> total    0.3989 0.0268 20000 0.3065 0.3271 0.3450 0.4502 0.4660 0.4822
```

## More Information

See [GitHub Pages](https://jeksterslab.github.io/semmcci/index.html) for
package documentation.
