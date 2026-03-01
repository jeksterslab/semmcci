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

  Numeric vector. Significance level \\\alpha\\.

- decomposition:

  Character string. Matrix decomposition of the sampling
  variance-covariance matrix for the data generation. If
  `decomposition = "chol"`, use Cholesky decomposition. If
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

  Parameter estimates \\\hat{\theta}\\.

- thetahatstar:

  Sampling distribution of parameter estimates \\\hat{\theta}^{\ast}\\.

- fun:

  Function used ("MCMI").

## Details

A sampling distribution of parameter estimates is generated from the
multivariate normal distribution using the parameter estimates and the
sampling variance-covariance matrix obtained using multiple imputation.
Confidence intervals for free and defined parameters are generated using
the simulated sampling distribution. Parameters can be defined using the
`:=` operator in the `lavaan` model syntax.

## References

Pesigan, I. J. A., & Cheung, S. F. (2024). Monte Carlo confidence
intervals for the indirect effect with missing data. *Behavior Research
Methods*.
[doi:10.3758/s13428-023-02114-4](https://doi.org/10.3758/s13428-023-02114-4)

Rubin, D. B. (1987). *Multiple imputation for nonresponse in surveys*.
John Wiley & Sons, Inc.

## See also

Other Monte Carlo in Structural Equation Modeling Functions:
[`Func()`](https://github.com/jeksterslab/semmcci/reference/Func.md),
[`MC()`](https://github.com/jeksterslab/semmcci/reference/MC.md),
[`MCFunc()`](https://github.com/jeksterslab/semmcci/reference/MCFunc.md),
[`MCGeneric()`](https://github.com/jeksterslab/semmcci/reference/MCGeneric.md),
[`MCStd()`](https://github.com/jeksterslab/semmcci/reference/MCStd.md)

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
#>                       est     se R   2.5%  97.5%
#> cp                 0.4189 0.2485 5 0.0534 0.6831
#> b                  0.4634 0.1046 5 0.3214 0.5797
#> a                  0.3026 0.2999 5 0.1018 0.7836
#> cond~~cond         0.2478 0.0294 5 0.2025 0.2678
#> reaction~~reaction 1.9811 0.1896 5 1.7173 2.1768
#> pmi~~pmi           1.7244 0.4269 5 1.4001 2.4033
#> indirect           0.1412 0.1381 5 0.0414 0.3567
#> direct             0.4189 0.2485 5 0.0534 0.6831
#> total              0.5601 0.1641 5 0.3496 0.7382
```
