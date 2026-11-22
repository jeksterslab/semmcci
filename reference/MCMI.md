# Monte Carlo Confidence Intervals (Multiple Imputation)

Calculates Monte Carlo confidence intervals for free and defined
parameters. Missing values are handled using multilple imputation.

## Usage

``` r
MCMI(
  lav,
  mi,
  R = 20000L,
  alpha = c(0.001, 0.01, 0.05),
  decomposition = "eigen",
  pd = TRUE,
  tol = 1e-06,
  seed = NULL
)
```

## Arguments

- lav:

  Object of class `lavaan`.

- mi:

  Object of class `mids` (output of
  [`mice::mice()`](https://amices.org/mice/reference/mice.html)), object
  of class `amelia` (output of
  [`Amelia::amelia()`](https://rdrr.io/pkg/Amelia/man/amelia.html)), or
  a list of multiply imputed data sets.

- R:

  Positive integer. Number of Monte Carlo replications.

- alpha:

  Numeric vector. Significance level \\(\alpha\\).

- decomposition:

  Character string. Matrix decomposition of the sampling
  variance-covariance matrix for the data generation. If
  `decomposition = "chol"`, use Cholesky decomposition; if the Cholesky
  decomposition fails (for example, a singular matrix from equality
  constraints), the eigenvalue decomposition is used. If
  `decomposition = "eigen"`, use eigenvalue decomposition. If
  `decomposition = "svd"`, use singular value decomposition.

- pd:

  Logical. If `pd = TRUE`, check if the sampling variance-covariance
  matrix is positive definite using `tol`.

- tol:

  Numeric. Tolerance used for `pd`.

- seed:

  Integer. Random seed for reproducibility.

## Value

Returns an object of class `semmcci` which is a list with the following
elements:

- call:

  Function call.

- args:

  List of function arguments.

- thetahat:

  Parameter estimates \\(\hat{\theta}\\).

- thetahatstar:

  Sampling distribution of parameter estimates
  \\(\hat{\theta}^{\ast}\\).

- fun:

  Function used ("MCMI").

## Details

A sampling distribution of parameter estimates is generated from the
multivariate normal distribution using the parameter estimates and the
sampling variance-covariance matrix obtained using multiple imputation.
Confidence intervals for free and defined parameters are generated using
the simulated sampling distribution. Parameters can be defined using the
`:=` operator in the `lavaan` model syntax.

Let \\(\hat{\boldsymbol{\theta}}\_{m}\\) and
\\(\hat{\boldsymbol{V}}\_{m}\\) be the estimates and their sampling
covariance matrix in imputed data set \\(m = 1, \dots, M\\). The pooled
estimates and covariance matrices (Rubin's rules) are
\$\$\bar{\boldsymbol{\theta}} = \frac{1}{M} \sum\_{m = 1}^{M}
\hat{\boldsymbol{\theta}}\_{m}, \quad \bar{\boldsymbol{W}} = \frac{1}{M}
\sum\_{m = 1}^{M} \hat{\boldsymbol{V}}\_{m}, \quad \boldsymbol{B} =
\frac{1}{M - 1} \sum\_{m = 1}^{M} \left(
\hat{\boldsymbol{\theta}}\_{m} - \bar{\boldsymbol{\theta}} \right)
\left( \hat{\boldsymbol{\theta}}\_{m} - \bar{\boldsymbol{\theta}}
\right)^{\prime},\$\$ \$\$\boldsymbol{T} = \bar{\boldsymbol{W}} + \left(
1 + \frac{1}{M} \right) \boldsymbol{B}.\$\$ The draws are generated as
in [`MC()`](https://jeksterslab.github.io/semmcci/reference/MC.md), with
\\(\bar{\boldsymbol{\theta}}\\) and \\(\boldsymbol{T}\\) in place of
\\(\hat{\boldsymbol{\theta}}\\) and \\(\hat{\boldsymbol{V}}\\). A
defined parameter \\(\phi = h \left( \boldsymbol{\theta} \right)\\),
given by the `:=` operator in the model syntax, has the draws
\\(\hat{\phi}^{\ast}\_{r} = h \left(
\hat{\boldsymbol{\theta}}^{\ast}\_{r} \right)\\). The confidence
intervals are the percentile intervals \\(\left\[ \hat{G}^{-1} \left(
\alpha / 2 \right), \hat{G}^{-1} \left( 1 - \alpha / 2 \right)
\right\]\\), where \\(\hat{G}\\) is the empirical distribution function
of the draws (Preacher & Selig, 2012).

## References

Pesigan, I. J. A., & Cheung, S. F. (2024). Monte Carlo confidence
intervals for the indirect effect with missing data. *Behavior Research
Methods*.
[doi:10.3758/s13428-023-02114-4](https://doi.org/10.3758/s13428-023-02114-4)

Rubin, D. B. (1987). *Multiple imputation for nonresponse in surveys*.
John Wiley & Sons, Inc.

Preacher, K. J., & Selig, J. P. (2012). Advantages of Monte Carlo
confidence intervals for indirect effects. *Communication Methods and
Measures*, *6*(2), 77–98.
[doi:10.1080/19312458.2012.679848](https://doi.org/10.1080/19312458.2012.679848)

## See also

Other Monte Carlo in Structural Equation Modeling Functions:
[`Func()`](https://jeksterslab.github.io/semmcci/reference/Func.md),
[`MC()`](https://jeksterslab.github.io/semmcci/reference/MC.md),
[`MCFunc()`](https://jeksterslab.github.io/semmcci/reference/MCFunc.md),
[`MCGeneric()`](https://jeksterslab.github.io/semmcci/reference/MCGeneric.md),
[`MCStd()`](https://jeksterslab.github.io/semmcci/reference/MCStd.md)

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
library(semmcci)
library(lavaan)

# Data ---------------------------------------------------------------------
data("Tal.Or", package = "psych")
df <- mice::ampute(Tal.Or)$amp

# Monte Carlo (Multiple Imputation) ----------------------------------------
## Multiple Imputation -----------------------------------------------------
mi <- mice::mice(
  data = df,
  print = FALSE,
  m = 5L, # use a large value e.g., 100L for actual research,
  seed = 42
)

## Fit Model in lavaan -----------------------------------------------------
model <- "
  reaction ~ cp * cond + b * pmi
  pmi ~ a * cond
  cond ~~ cond
  indirect := a * b
  direct := cp
  total := cp + (a * b)
"
fit <- sem(data = df, model = model) # use default listwise deletion

## MCMI() ------------------------------------------------------------------
MCMI(
  fit,
  mi = mi,
  R = 5L, # use a large value e.g., 20000L for actual research
  alpha = 0.05
)
#> Monte Carlo Confidence Intervals (Multiple Imputation Estimates)
#>                       est     se R    2.5%  97.5%
#> cp                 0.1898 0.2120 5 -0.2329 0.2508
#> b                  0.4956 0.0457 5  0.3583 0.4688
#> a                  0.5468 0.2241 5  0.5814 1.0848
#> cond~~cond         0.2491 0.0454 5  0.1904 0.2961
#> reaction~~reaction 1.9358 0.1588 5  1.8555 2.1767
#> pmi~~pmi           1.6766 0.2001 5  1.3038 1.7446
#> indirect           0.2708 0.0759 5  0.2451 0.4194
#> direct             0.1898 0.2120 5 -0.2329 0.2508
#> total              0.4606 0.2264 5  0.1693 0.6340
```
