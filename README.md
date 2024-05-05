semmcci
================
Ivan Jacob Agaloos Pesigan
2024-05-05

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
#>  Min.   :-3.15624   Min.   :-2.73869   Min.   :-2.69669  
#>  1st Qu.:-0.72450   1st Qu.:-0.65170   1st Qu.:-0.62151  
#>  Median :-0.02426   Median :-0.03393   Median : 0.01553  
#>  Mean   :-0.04016   Mean   :-0.03028   Mean   : 0.02216  
#>  3rd Qu.: 0.60774   3rd Qu.: 0.57818   3rd Qu.: 0.65129  
#>  Max.   : 3.67623   Max.   : 3.16891   Max.   : 3.05663  
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
#> cp        0.2419 0.0323 20000  0.1788 0.3062
#> b         0.5068 0.0325 20000  0.4432 0.5708
#> a         0.4988 0.0283 20000  0.4432 0.5540
#> X~~X      0.9564 0.0447 20000  0.8675 1.0437
#> Y~~Y      0.5659 0.0275 20000  0.5124 0.6202
#> M~~M      0.6536 0.0316 20000  0.5906 0.7145
#> Y~1       0.0468 0.0256 20000 -0.0038 0.0967
#> M~1      -0.0102 0.0272 20000 -0.0632 0.0434
#> X~1      -0.0430 0.0321 20000 -0.1051 0.0210
#> indirect  0.2528 0.0217 20000  0.2116 0.2963
#> direct    0.2419 0.0323 20000  0.1788 0.3062
#> total     0.4947 0.0302 20000  0.4352 0.5534
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
#> Warning in check_dep_version(): ABI version mismatch: 
#> lme4 was built with Matrix ABI version 1
#> Current Matrix ABI version is 2
#> Please re-install lme4 from source or restore original 'Matrix' package
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
#> cp       0.2408 0.0320 20000 0.1785 0.3036
#> b        0.5049 0.0331 20000 0.4404 0.5695
#> a        0.4980 0.0283 20000 0.4422 0.5536
#> X~~X     0.9571 0.0449 20000 0.8696 1.0457
#> Y~~Y     0.5668 0.0277 20000 0.5128 0.6217
#> M~~M     0.6531 0.0312 20000 0.5917 0.7139
#> indirect 0.2515 0.0218 20000 0.2101 0.2957
#> direct   0.2408 0.0320 20000 0.1785 0.3036
#> total    0.4922 0.0303 20000 0.4330 0.5511
```

### Standardized Monte Carlo Confidence Intervals

Standardized Monte Carlo Confidence intervals can be generated by
passing the result of the `MC()` function or the `MCMI()` function to
`MCStd()`.

``` r
MCStd(mc, alpha = 0.05)
#> Standardized Monte Carlo Confidence Intervals
#>              est     se     R   2.5%  97.5%
#> cp        0.2405 0.0315 20000 0.1786 0.3024
#> b         0.4864 0.0289 20000 0.4289 0.5420
#> a         0.5166 0.0255 20000 0.4649 0.5652
#> X~~X      1.0000 0.0000 20000 1.0000 1.0000
#> Y~~Y      0.5847 0.0260 20000 0.5333 0.6345
#> M~~M      0.7331 0.0262 20000 0.6805 0.7838
#> indirect  0.0476 0.0197 20000 0.2131 0.2900
#> direct   -0.0108 0.0315 20000 0.1786 0.3024
#> total    -0.0440 0.0261 20000 0.4389 0.5413
```

``` r
MCStd(mcmi, alpha = 0.05)
#> Standardized Monte Carlo Confidence Intervals
#>             est     se     R   2.5%  97.5%
#> cp       0.2419 0.0312 20000 0.1779 0.3005
#> b        0.4847 0.0291 20000 0.4272 0.5410
#> a        0.5023 0.0253 20000 0.4657 0.5654
#> X~~X     1.0000 0.0000 20000 1.0000 1.0000
#> Y~~Y     0.5888 0.0265 20000 0.5349 0.6387
#> M~~M     0.7477 0.0261 20000 0.6803 0.7831
#> indirect 0.2434 0.0199 20000 0.2118 0.2903
#> direct   0.2419 0.0312 20000 0.1779 0.3005
#> total    0.4853 0.0261 20000 0.4375 0.5390
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
