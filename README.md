semmcci
================
Ivan Jacob Agaloos Pesigan
2023-09-05

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
#>  Min.   :-3.29557   Min.   :-3.01715   Min.   :-3.24434  
#>  1st Qu.:-0.65317   1st Qu.:-0.62548   1st Qu.:-0.63119  
#>  Median :-0.02895   Median : 0.02606   Median : 0.02543  
#>  Mean   :-0.00525   Mean   : 0.03643   Mean   : 0.03886  
#>  3rd Qu.: 0.71131   3rd Qu.: 0.70256   3rd Qu.: 0.68914  
#>  Max.   : 3.17898   Max.   : 3.05157   Max.   : 3.21915  
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
#> cp        0.2250 0.0312 20000  0.1637 0.2858
#> b         0.4740 0.0310 20000  0.4126 0.5343
#> a         0.4785 0.0303 20000  0.4192 0.5384
#> X~~X      0.9871 0.0467 20000  0.8968 1.0795
#> Y~~Y      0.5853 0.0284 20000  0.5300 0.6404
#> M~~M      0.7653 0.0366 20000  0.6933 0.8366
#> Y~1       0.0222 0.0259 20000 -0.0279 0.0735
#> M~1       0.0415 0.0292 20000 -0.0153 0.0990
#> X~1      -0.0043 0.0326 20000 -0.0686 0.0595
#> indirect  0.2268 0.0205 20000  0.1880 0.2681
#> direct    0.2250 0.0312 20000  0.1637 0.2858
#> total     0.4518 0.0299 20000  0.3934 0.5111
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
#> cp       0.2212 0.0317 20000 0.1592 0.2836
#> b        0.4754 0.0318 20000 0.4129 0.5375
#> a        0.4791 0.0301 20000 0.4193 0.5381
#> X~~X     0.9885 0.0469 20000 0.8962 1.0801
#> Y~~Y     0.5842 0.0284 20000 0.5288 0.6400
#> M~~M     0.7665 0.0366 20000 0.6936 0.8378
#> indirect 0.2278 0.0211 20000 0.1873 0.2696
#> direct   0.2212 0.0317 20000 0.1592 0.2836
#> total    0.4490 0.0293 20000 0.3918 0.5066
```

### Standardized Monte Carlo Confidence Intervals

Standardized Monte Carlo Confidence intervals can be generated by
passing the result of the `MC()` function or the `MCMI()` function to
`MCStd()`.

``` r
MCStd(mc, alpha = 0.05)
#> Standardized Monte Carlo Confidence Intervals
#>              est     se     R   2.5%  97.5%
#> cp        0.2283 0.0313 20000 0.1667 0.2891
#> b         0.4819 0.0287 20000 0.4246 0.5368
#> a         0.4774 0.0269 20000 0.4238 0.5295
#> X~~X      1.0000 0.0000 20000 1.0000 1.0000
#> Y~~Y      0.6105 0.0263 20000 0.5583 0.6609
#> M~~M      0.7721 0.0256 20000 0.7196 0.8204
#> indirect  0.0227 0.0191 20000 0.1933 0.2677
#> direct    0.0416 0.0313 20000 0.1667 0.2891
#> total    -0.0043 0.0272 20000 0.4039 0.5109
```

``` r
MCStd(mcmi, alpha = 0.05)
#> Standardized Monte Carlo Confidence Intervals
#>             est     se     R   2.5%  97.5%
#> cp       0.2264 0.0321 20000 0.1616 0.2876
#> b        0.4734 0.0297 20000 0.4255 0.5413
#> a        0.4605 0.0268 20000 0.4239 0.5292
#> X~~X     1.0000 0.0000 20000 1.0000 1.0000
#> Y~~Y     0.6259 0.0259 20000 0.5593 0.6606
#> M~~M     0.7880 0.0256 20000 0.7199 0.8203
#> indirect 0.2180 0.0196 20000 0.1933 0.2702
#> direct   0.2264 0.0321 20000 0.1616 0.2876
#> total    0.4444 0.0270 20000 0.4023 0.5079
```

## Documentation

See [GitHub Pages](https://jeksterslab.github.io/semmcci/index.html) for
package documentation.

## Citation

To cite `semmcci` in publications, please cite Pesigan & Cheung (2023).

## References

<div id="refs" class="references csl-bib-body hanging-indent"
line-spacing="2">

<div id="ref-MacKinnon-Lockwood-Williams-2004" class="csl-entry">

MacKinnon, D. P., Lockwood, C. M., & Williams, J. (2004). Confidence
limits for the indirect effect: Distribution of the product and
resampling methods. *Multivariate Behavioral Research*, *39*(1), 99–128.
<https://doi.org/10.1207/s15327906mbr3901_4>

</div>

<div id="ref-Pesigan-Cheung-2023" class="csl-entry">

Pesigan, I. J. A., & Cheung, S. F. (2023). Monte Carlo confidence
intervals for the indirect effect with missing data. *Behavior Research
Methods*. <https://doi.org/10.3758/s13428-023-02114-4>

</div>

<div id="ref-Preacher-Selig-2012" class="csl-entry">

Preacher, K. J., & Selig, J. P. (2012). Advantages of monte carlo
confidence intervals for indirect effects. *Communication Methods and
Measures*, *6*(2), 77–98. <https://doi.org/10.1080/19312458.2012.679848>

</div>

<div id="ref-Tofighi-Kelley-2019" class="csl-entry">

Tofighi, D., & Kelley, K. (2019). Indirect effects in sequential
mediation models: Evaluating methods for hypothesis testing and
confidence interval formation. *Multivariate Behavioral Research*,
*55*(2), 188–210. <https://doi.org/10.1080/00273171.2019.1618545>

</div>

<div id="ref-Tofighi-MacKinnon-2015" class="csl-entry">

Tofighi, D., & MacKinnon, D. P. (2015). Monte Carlo confidence intervals
for complex functions of indirect effects. *Structural Equation
Modeling: A Multidisciplinary Journal*, *23*(2), 194–205.
<https://doi.org/10.1080/10705511.2015.1057284>

</div>

</div>
