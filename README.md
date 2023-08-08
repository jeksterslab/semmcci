semmcci
================
Ivan Jacob Agaloos Pesigan
2023-08-08

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
#>  Min.   :-4.08024   Min.   :-2.85992   Min.   :-3.45490  
#>  1st Qu.:-0.65910   1st Qu.:-0.67969   1st Qu.:-0.56484  
#>  Median :-0.01140   Median : 0.03271   Median : 0.02257  
#>  Mean   :-0.03456   Mean   : 0.03400   Mean   : 0.03935  
#>  3rd Qu.: 0.62021   3rd Qu.: 0.71500   3rd Qu.: 0.68422  
#>  Max.   : 2.98641   Max.   : 3.09469   Max.   : 2.90955  
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
#> cp        0.2103 0.0306 20000  0.1501 0.2694
#> b         0.4896 0.0291 20000  0.4326 0.5473
#> a         0.5036 0.0311 20000  0.4420 0.5636
#> X~~X      0.9543 0.0450 20000  0.8670 1.0437
#> Y~~Y      0.5492 0.0268 20000  0.4967 0.6018
#> M~~M      0.7860 0.0377 20000  0.7121 0.8596
#> Y~1       0.0243 0.0252 20000 -0.0254 0.0737
#> M~1       0.0455 0.0293 20000 -0.0113 0.1025
#> X~1      -0.0279 0.0317 20000 -0.0894 0.0344
#> indirect  0.2465 0.0211 20000  0.2064 0.2891
#> direct    0.2103 0.0306 20000  0.1501 0.2694
#> total     0.4568 0.0299 20000  0.3973 0.5155
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
#> cp       0.2084 0.0304 20000 0.1493 0.2679
#> b        0.4890 0.0289 20000 0.4328 0.5458
#> a        0.5038 0.0312 20000 0.4426 0.5649
#> X~~X     0.9553 0.0454 20000 0.8663 1.0442
#> Y~~Y     0.5488 0.0274 20000 0.4950 0.6023
#> M~~M     0.7865 0.0387 20000 0.7115 0.8623
#> indirect 0.2463 0.0209 20000 0.2072 0.2889
#> direct   0.2084 0.0304 20000 0.1493 0.2679
#> total    0.4547 0.0299 20000 0.3961 0.5131
```

### Standardized Monte Carlo Confidence Intervals

Standardized Monte Carlo Confidence intervals can be generated by
passing the result of the `MC()` function or the `MCMI()` function to
`MCStd()`.

``` r
MCStd(mc, alpha = 0.05)
#> Standardized Monte Carlo Confidence Intervals
#>              est     se     R   2.5%  97.5%
#> cp        0.2123 0.0306 20000 0.1517 0.2707
#> b         0.5129 0.0276 20000 0.4576 0.5662
#> a         0.4852 0.0263 20000 0.4332 0.5351
#> X~~X      1.0000 0.0000 20000 1.0000 1.0000
#> Y~~Y      0.5863 0.0258 20000 0.5344 0.6365
#> M~~M      0.7646 0.0255 20000 0.7137 0.8124
#> indirect  0.0251 0.0193 20000 0.2111 0.2870
#> direct    0.0448 0.0306 20000 0.1517 0.2707
#> total    -0.0285 0.0268 20000 0.4060 0.5124
```

``` r
MCStd(mcmi, alpha = 0.05)
#> Standardized Monte Carlo Confidence Intervals
#>             est     se     R   2.5%  97.5%
#> cp       0.2124 0.0303 20000 0.1517 0.2703
#> b        0.5162 0.0279 20000 0.4577 0.5671
#> a        0.4710 0.0269 20000 0.4308 0.5358
#> X~~X     1.0000 0.0000 20000 1.0000 1.0000
#> Y~~Y     0.5851 0.0263 20000 0.5346 0.6376
#> M~~M     0.7781 0.0260 20000 0.7129 0.8144
#> indirect 0.2431 0.0193 20000 0.2119 0.2877
#> direct   0.2124 0.0303 20000 0.1517 0.2703
#> total    0.4556 0.0271 20000 0.4057 0.5113
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
