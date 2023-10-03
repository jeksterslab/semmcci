semmcci
================
Ivan Jacob Agaloos Pesigan
2023-10-03

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
#>  Min.   :-4.23562   Min.   :-2.98867   Min.   :-3.55634  
#>  1st Qu.:-0.67841   1st Qu.:-0.64826   1st Qu.:-0.68539  
#>  Median :-0.01618   Median :-0.02148   Median :-0.07007  
#>  Mean   :-0.01414   Mean   :-0.02149   Mean   :-0.03472  
#>  3rd Qu.: 0.68349   3rd Qu.: 0.69698   3rd Qu.: 0.63702  
#>  Max.   : 2.91433   Max.   : 3.70370   Max.   : 3.15633  
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
#> cp        0.2957 0.0291 20000  0.2388 0.3535
#> b         0.4649 0.0295 20000  0.4065 0.5229
#> a         0.4788 0.0289 20000  0.4226 0.5347
#> X~~X      1.0012 0.0471 20000  0.9096 1.0935
#> Y~~Y      0.5074 0.0247 20000  0.4592 0.5560
#> M~~M      0.7299 0.0344 20000  0.6628 0.7975
#> Y~1      -0.0141 0.0243 20000 -0.0607 0.0335
#> M~1      -0.0170 0.0287 20000 -0.0736 0.0391
#> X~1      -0.0118 0.0329 20000 -0.0764 0.0520
#> indirect  0.2226 0.0193 20000  0.1859 0.2618
#> direct    0.2957 0.0291 20000  0.2388 0.3535
#> total     0.5183 0.0279 20000  0.4636 0.5740
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
#> cp       0.2959 0.0284 20000 0.2405 0.3517
#> b        0.4643 0.0299 20000 0.4057 0.5234
#> a        0.4786 0.0291 20000 0.4213 0.5358
#> X~~X     1.0024 0.0484 20000 0.9078 1.0974
#> Y~~Y     0.5074 0.0249 20000 0.4593 0.5561
#> M~~M     0.7302 0.0348 20000 0.6617 0.7983
#> indirect 0.2222 0.0193 20000 0.1848 0.2609
#> direct   0.2959 0.0284 20000 0.2405 0.3517
#> total    0.5181 0.0273 20000 0.4645 0.5722
```

### Standardized Monte Carlo Confidence Intervals

Standardized Monte Carlo Confidence intervals can be generated by
passing the result of the `MC()` function or the `MCMI()` function to
`MCStd()`.

``` r
MCStd(mc, alpha = 0.05)
#> Standardized Monte Carlo Confidence Intervals
#>              est     se     R   2.5%  97.5%
#> cp        0.3061 0.0296 20000 0.2473 0.3640
#> b         0.4712 0.0276 20000 0.4157 0.5238
#> a         0.4892 0.0260 20000 0.4368 0.5383
#> X~~X      1.0000 0.0000 20000 1.0000 1.0000
#> Y~~Y      0.5432 0.0251 20000 0.4941 0.5926
#> M~~M      0.7607 0.0254 20000 0.7102 0.8092
#> indirect -0.0146 0.0181 20000 0.1953 0.2663
#> direct   -0.0174 0.0296 20000 0.2473 0.3640
#> total    -0.0118 0.0248 20000 0.4863 0.5840
```

``` r
MCStd(mcmi, alpha = 0.05)
#> Standardized Monte Carlo Confidence Intervals
#>             est     se     R   2.5%  97.5%
#> cp       0.2978 0.0291 20000 0.2493 0.3632
#> b        0.4696 0.0276 20000 0.4158 0.5242
#> a        0.4940 0.0262 20000 0.4368 0.5395
#> X~~X     1.0000 0.0000 20000 1.0000 1.0000
#> Y~~Y     0.5526 0.0254 20000 0.4939 0.5928
#> M~~M     0.7560 0.0256 20000 0.7090 0.8092
#> indirect 0.2320 0.0182 20000 0.1948 0.2662
#> direct   0.2978 0.0291 20000 0.2493 0.3632
#> total    0.5298 0.0245 20000 0.4871 0.5838
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
