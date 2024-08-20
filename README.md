semmcci
================
Ivan Jacob Agaloos Pesigan
2024-08-20

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
#>  Min.   :-4.00294   Min.   :-3.35169   Min.   :-3.20074  
#>  1st Qu.:-0.57582   1st Qu.:-0.72116   1st Qu.:-0.61172  
#>  Median : 0.07574   Median :-0.06034   Median : 0.10346  
#>  Mean   : 0.04190   Mean   :-0.04597   Mean   : 0.06846  
#>  3rd Qu.: 0.69890   3rd Qu.: 0.62467   3rd Qu.: 0.74076  
#>  Max.   : 3.51560   Max.   : 2.98118   Max.   : 2.94785  
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
#>              est     se     R    2.5%   97.5%
#> cp        0.2151 0.0320 20000  0.1514  0.2772
#> b         0.5042 0.0322 20000  0.4411  0.5676
#> a         0.4938 0.0299 20000  0.4352  0.5521
#> X~~X      0.9773 0.0459 20000  0.8883  1.0684
#> Y~~Y      0.6097 0.0301 20000  0.5511  0.6681
#> M~~M      0.7513 0.0362 20000  0.6796  0.8235
#> Y~1       0.0808 0.0267 20000  0.0292  0.1322
#> M~1      -0.0641 0.0291 20000 -0.1206 -0.0073
#> X~1       0.0398 0.0323 20000 -0.0241  0.1030
#> indirect  0.2490 0.0217 20000  0.2081  0.2933
#> direct    0.2151 0.0320 20000  0.1514  0.2772
#> total     0.4641 0.0307 20000  0.4032  0.5238
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
#> cp       0.2156 0.0326 20000 0.1520 0.2795
#> b        0.5029 0.0318 20000 0.4405 0.5653
#> a        0.4928 0.0305 20000 0.4331 0.5528
#> X~~X     0.9783 0.0466 20000 0.8872 1.0698
#> Y~~Y     0.6089 0.0299 20000 0.5501 0.6675
#> M~~M     0.7524 0.0361 20000 0.6805 0.8227
#> indirect 0.2479 0.0223 20000 0.2056 0.2927
#> direct   0.2156 0.0326 20000 0.1520 0.2795
#> total    0.4634 0.0310 20000 0.4027 0.5245
```

### Standardized Monte Carlo Confidence Intervals

Standardized Monte Carlo Confidence intervals can be generated by
passing the result of the `MC()` function or the `MCMI()` function to
`MCStd()`.

``` r
MCStd(mc, alpha = 0.05)
#> Standardized Monte Carlo Confidence Intervals
#>              est     se     R   2.5%  97.5%
#> cp        0.2115 0.0311 20000 0.1493 0.2709
#> b         0.4988 0.0289 20000 0.4407 0.5539
#> a         0.4907 0.0262 20000 0.4382 0.5408
#> X~~X      1.0000 0.0000 20000 1.0000 1.0000
#> Y~~Y      0.6030 0.0266 20000 0.5499 0.6544
#> M~~M      0.7592 0.0257 20000 0.7076 0.8080
#> indirect  0.0804 0.0194 20000 0.2075 0.2836
#> direct   -0.0644 0.0311 20000 0.1493 0.2709
#> total     0.0403 0.0270 20000 0.4015 0.5066
```

``` r
MCStd(mcmi, alpha = 0.05)
#> Standardized Monte Carlo Confidence Intervals
#>             est     se     R   2.5%  97.5%
#> cp       0.2302 0.0317 20000 0.1493 0.2736
#> b        0.4847 0.0291 20000 0.4400 0.5534
#> a        0.4715 0.0267 20000 0.4367 0.5410
#> X~~X     1.0000 0.0000 20000 1.0000 1.0000
#> Y~~Y     0.6068 0.0266 20000 0.5500 0.6546
#> M~~M     0.7777 0.0262 20000 0.7073 0.8093
#> indirect 0.2286 0.0201 20000 0.2049 0.2839
#> direct   0.2302 0.0317 20000 0.1493 0.2736
#> total    0.4588 0.0273 20000 0.4014 0.5089
```

## Documentation

See [GitHub Pages](https://jeksterslab.github.io/semmcci/index.html) for
package documentation.

## Citation

To cite `semmcci` in publications, please cite Pesigan & Cheung (2023).

## References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0" line-spacing="2">

<div id="ref-MacKinnon-Lockwood-Williams-2004" class="csl-entry">

MacKinnon, D. P., Lockwood, C. M., & Williams, J. (2004). Confidence
limits for the indirect effect: Distribution of the product and
resampling methods. *Multivariate Behavioral Research*, *39*(1), 99–128.
<https://doi.org/10.1207/s15327906mbr3901_4>

</div>

<div id="ref-Pesigan-Cheung-2023" class="csl-entry">

Pesigan, I. J. A., & Cheung, S. F. (2023). Monte Carlo confidence
intervals for the indirect effect with missing data. *Behavior Research
Methods*, *56*(3), 1678–1696.
<https://doi.org/10.3758/s13428-023-02114-4>

</div>

<div id="ref-Preacher-Selig-2012" class="csl-entry">

Preacher, K. J., & Selig, J. P. (2012). Advantages of Monte Carlo
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
