semmcci
================
Ivan Jacob Agaloos Pesigan
2025-07-22

<!-- README.md is generated from .setup/readme/README.Rmd. Please edit that file -->

<!-- badges: start -->

[![CRAN
Status](https://www.r-pkg.org/badges/version/semmcci)](https://cran.r-project.org/package=semmcci)
[![R-Universe
Status](https://jeksterslab.r-universe.dev/badges/semmcci)](https://jeksterslab.r-universe.dev/semmcci)
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
#>        X                  M                   Y           
#>  Min.   :-3.15272   Min.   :-2.770180   Min.   :-3.15166  
#>  1st Qu.:-0.68013   1st Qu.:-0.617813   1st Qu.:-0.67360  
#>  Median : 0.02779   Median :-0.008015   Median :-0.03968  
#>  Mean   : 0.00338   Mean   : 0.016066   Mean   :-0.02433  
#>  3rd Qu.: 0.69009   3rd Qu.: 0.668327   3rd Qu.: 0.63753  
#>  Max.   : 2.90756   Max.   : 2.712008   Max.   : 3.27343  
#>  NA's   :100        NA's   :100         NA's   :100
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
#> cp        0.2491 0.0311 20000  0.1872 0.3099
#> b         0.4685 0.0321 20000  0.4064 0.5318
#> a         0.5032 0.0274 20000  0.4500 0.5562
#> X~~X      1.0556 0.0496 20000  0.9573 1.1532
#> Y~~Y      0.5830 0.0284 20000  0.5279 0.6393
#> M~~M      0.6790 0.0328 20000  0.6133 0.7427
#> Y~1      -0.0307 0.0258 20000 -0.0817 0.0204
#> M~1       0.0134 0.0278 20000 -0.0410 0.0675
#> X~1       0.0029 0.0337 20000 -0.0634 0.0695
#> indirect  0.2358 0.0206 20000  0.1973 0.2777
#> direct    0.2491 0.0311 20000  0.1872 0.3099
#> total     0.4849 0.0284 20000  0.4294 0.5406
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
#> cp       0.2479 0.0311 20000 0.1874 0.3086
#> b        0.4689 0.0332 20000 0.4041 0.5338
#> a        0.5031 0.0275 20000 0.4499 0.5572
#> X~~X     1.0568 0.0502 20000 0.9587 1.1554
#> Y~~Y     0.5827 0.0289 20000 0.5257 0.6396
#> M~~M     0.6794 0.0328 20000 0.6151 0.7433
#> indirect 0.2359 0.0207 20000 0.1967 0.2775
#> direct   0.2479 0.0311 20000 0.1874 0.3086
#> total    0.4838 0.0284 20000 0.4276 0.5396
```

### Standardized Monte Carlo Confidence Intervals

Standardized Monte Carlo Confidence intervals can be generated by
passing the result of the `MC()` function or the `MCMI()` function to
`MCStd()`.

``` r
MCStd(mc, alpha = 0.05)
#> Standardized Monte Carlo Confidence Intervals
#>              est     se     R   2.5%  97.5%
#> cp        0.2585 0.0318 20000 0.1951 0.3204
#> b         0.4603 0.0296 20000 0.4017 0.5178
#> a         0.5315 0.0247 20000 0.4824 0.5791
#> X~~X      1.0000 0.0000 20000 1.0000 1.0000
#> Y~~Y      0.5948 0.0260 20000 0.5431 0.6449
#> M~~M      0.7175 0.0262 20000 0.6646 0.7672
#> indirect -0.0310 0.0196 20000 0.2072 0.2838
#> direct    0.0137 0.0318 20000 0.1951 0.3204
#> total     0.0028 0.0257 20000 0.4508 0.5515
```

``` r
MCStd(mcmi, alpha = 0.05)
#> Standardized Monte Carlo Confidence Intervals
#>             est     se     R   2.5%  97.5%
#> cp       0.2626 0.0320 20000 0.1953 0.3195
#> b        0.4461 0.0304 20000 0.4004 0.5190
#> a        0.5187 0.0250 20000 0.4819 0.5799
#> X~~X     1.0000 0.0000 20000 1.0000 1.0000
#> Y~~Y     0.6105 0.0265 20000 0.5419 0.6460
#> M~~M     0.7310 0.0265 20000 0.6637 0.7678
#> indirect 0.2314 0.0196 20000 0.2071 0.2836
#> direct   0.2626 0.0320 20000 0.1953 0.3195
#> total    0.4940 0.0260 20000 0.4503 0.5519
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
