semmcci
================
Ivan Jacob Agaloos Pesigan
2023-08-10

<!-- README.md is generated from .setup/readme/README.Rmd. Please edit that file -->
<!-- badges: start -->

[![CRAN
Status](https://www.r-pkg.org/badges/version/semmcci)](https://cran.r-project.org/package=semmcci)
[![R-Universe
Status](https://jeksterslab.r-universe.dev/badges/semmcci)](https://jeksterslab.r-universe.dev)
[![DOI](https://zenodo.org/badge/DOI/10.3758/s13428-023-02114-4.svg)](https://doi.org/10.3758/s13428-023-02114-4)
[![Make
Project](https://github.com/jeksterslab/semmcci/actions/workflows/make.yml/badge.svg)](https://github.com/jeksterslab/semmcci/actions/workflows/make.yml)
[![R-CMD-check](https://github.com/jeksterslab/semmcci/actions/workflows/check-full.yml/badge.svg)](https://github.com/jeksterslab/semmcci/actions/workflows/check-full.yml)
[![R Package Test
Coverage](https://github.com/jeksterslab/semmcci/actions/workflows/test-coverage.yml/badge.svg)](https://github.com/jeksterslab/semmcci/actions/workflows/test-coverage.yml)
[![Lint R
Package](https://github.com/jeksterslab/semmcci/actions/workflows/lint.yml/badge.svg)](https://github.com/jeksterslab/semmcci/actions/workflows/lint.yml)
[![Package Website (GitHub
Pages)](https://github.com/jeksterslab/semmcci/actions/workflows/pkgdown-gh-pages.yml/badge.svg)](https://github.com/jeksterslab/semmcci/actions/workflows/pkgdown-gh-pages.yml)
[![Compile
LaTeX](https://github.com/jeksterslab/semmcci/actions/workflows/latex.yml/badge.svg)](https://github.com/jeksterslab/semmcci/actions/workflows/latex.yml)
[![Shell
Check](https://github.com/jeksterslab/semmcci/actions/workflows/shellcheck.yml/badge.svg)](https://github.com/jeksterslab/semmcci/actions/workflows/shellcheck.yml)
[![pages-build-deployment](https://github.com/jeksterslab/semmcci/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/jeksterslab/semmcci/actions/workflows/pages/pages-build-deployment)
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
be generated using the `semmcci` package. The package has three main
functions, namely, `MC()`, `MCMI()`, and `MCStd()`. The output of
`lavaan` is passed as the first argument to the `MC()` function or the
`MCMI()` function to generate Monte Carlo confidence intervals. Monte
Carlo confidence intervals for the standardized estimates can also be
generated by passing the output of the `MC()` function or the `MCMI()`
function to the `MCStd()` function. A description of the package and
code examples are presented in Pesigan and Cheung (2023:
<https://doi.org/10.3758/s13428-023-02114-4>).

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
summary(df)
#>        X                  M                  Y           
#>  Min.   :-3.47540   Min.   :-3.26515   Min.   :-2.81954  
#>  1st Qu.:-0.68049   1st Qu.:-0.68964   1st Qu.:-0.70210  
#>  Median :-0.01692   Median : 0.02064   Median :-0.00984  
#>  Mean   : 0.00719   Mean   : 0.01423   Mean   : 0.00541  
#>  3rd Qu.: 0.71726   3rd Qu.: 0.68890   3rd Qu.: 0.67698  
#>  Max.   : 3.03728   Max.   : 3.00575   Max.   : 3.31082  
#>  NA's   :100        NA's   :100        NA's   :100
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
  X ~~ X
  indirect := a * b
  direct := cp
  total := cp + (a * b)
"
```

### Monte Carlo Confidence Intervals

We can now fit the model using the `sem()` function from `lavaan`. We
use full-information maximum likelihood to deal with missing values.

``` r
fit <- sem(data = df, model = model, missing = "fiml")
```

The `fit` `lavaan` object can then be passed to the `MC()` function to
generate Monte Carlo confidence intervals.

``` r
mc <- MC(fit, R = 20000L, alpha = 0.05)
mc
#> Monte Carlo Confidence Intervals
#>              est     se     R    2.5%  97.5%
#> cp        0.2421 0.0316 20000  0.1800 0.3038
#> b         0.4978 0.0308 20000  0.4378 0.5586
#> a         0.5335 0.0289 20000  0.4776 0.5903
#> X~~X      1.0326 0.0482 20000  0.9388 1.1268
#> Y~~Y      0.5646 0.0275 20000  0.5104 0.6178
#> M~~M      0.7458 0.0358 20000  0.6764 0.8165
#> Y~1      -0.0083 0.0255 20000 -0.0583 0.0411
#> M~1       0.0119 0.0287 20000 -0.0442 0.0688
#> X~1       0.0043 0.0336 20000 -0.0619 0.0696
#> indirect  0.2656 0.0220 20000  0.2238 0.3098
#> direct    0.2421 0.0316 20000  0.1800 0.3038
#> total     0.5077 0.0293 20000  0.4506 0.5650
```

### Monte Carlo Confidence Intervals - Multiple Imputation

The `MCMI()` function can be used to handle missing values using
multiple imputation. The `MCMI()` accepts the output of `mice::mice()`,
`Amelia::amelia()`, or a list of multiply imputed data sets. In this
example, we impute multivariate missing data under the normal model.

``` r
mi <- mice::mice(
  df,
  method = "norm",
  m = 100,
  print = FALSE,
  seed = 42
)
```

We fit the model using lavaan using the default listwise deletion.

``` r
fit <- sem(data = df, model = model)
```

The `fit` `lavaan` object and `mi` object can then be passed to the
`MCMI()` function to generate Monte Carlo confidence intervals.

``` r
mcmi <- MCMI(fit, mi = mi, R = 20000L, alpha = 0.05, seed = 42)
mcmi
#> Monte Carlo Confidence Intervals (Multiple Imputation Estimates)
#>             est     se     R   2.5%  97.5%
#> cp       0.2423 0.0309 20000 0.1819 0.3030
#> b        0.4963 0.0309 20000 0.4356 0.5566
#> a        0.5330 0.0286 20000 0.4763 0.5893
#> X~~X     1.0344 0.0485 20000 0.9397 1.1298
#> Y~~Y     0.5647 0.0277 20000 0.5108 0.6188
#> M~~M     0.7453 0.0350 20000 0.6761 0.8133
#> indirect 0.2645 0.0219 20000 0.2225 0.3082
#> direct   0.2423 0.0309 20000 0.1819 0.3030
#> total    0.5069 0.0287 20000 0.4512 0.5637
```

### Standardized Monte Carlo Confidence Intervals

Standardized Monte Carlo Confidence intervals can be generated by
passing the result of the `MC()` function or the `MCMI()` function to
`MCStd()`.

``` r
MCStd(mc, alpha = 0.05)
#> Standardized Monte Carlo Confidence Intervals
#>              est     se     R   2.5%  97.5%
#> cp        0.2441 0.0313 20000 0.1823 0.3047
#> b         0.5037 0.0288 20000 0.4470 0.5590
#> a         0.5316 0.0244 20000 0.4831 0.5788
#> X~~X      1.0000 0.0000 20000 1.0000 1.0000
#> Y~~Y      0.5560 0.0254 20000 0.5057 0.6056
#> M~~M      0.7174 0.0260 20000 0.6649 0.7666
#> indirect -0.0083 0.0201 20000 0.2291 0.3075
#> direct    0.0117 0.0313 20000 0.1823 0.3047
#> total     0.0043 0.0254 20000 0.4611 0.5600
```

``` r
MCStd(mcmi, alpha = 0.05)
#> Standardized Monte Carlo Confidence Intervals
#>             est     se     R   2.5%  97.5%
#> cp       0.2319 0.0310 20000 0.1836 0.3046
#> b        0.5177 0.0286 20000 0.4456 0.5576
#> a        0.5426 0.0242 20000 0.4833 0.5779
#> X~~X     1.0000 0.0000 20000 1.0000 1.0000
#> Y~~Y     0.5480 0.0254 20000 0.5079 0.6067
#> M~~M     0.7056 0.0257 20000 0.6661 0.7664
#> indirect 0.2809 0.0200 20000 0.2281 0.3063
#> direct   0.2319 0.0310 20000 0.1836 0.3046
#> total    0.5128 0.0251 20000 0.4612 0.5597
```

## Documentation

See [GitHub Pages](https://jeksterslab.github.io/semmcci/index.html) for
package documentation.

## Citation

To cite `semmcci` in publications, please cite Pesigan & Cheung (2023).

<div id="refs" class="references csl-bib-body hanging-indent"
line-spacing="2">

<div id="ref-Pesigan-Cheung-2023" class="csl-entry">

Pesigan, I. J. A., & Cheung, S. F. (2023). Monte Carlo confidence
intervals for the indirect effect with missing data. *Behavior Research
Methods*. <https://doi.org/10.3758/s13428-023-02114-4>

</div>

</div>
