# Monte Carlo Confidence Intervals (Generic)

Calculates Monte Carlo confidence intervals for defined parameters for
any fitted model object with `coef` and `vcov` methods.

## Usage

``` r
MCGeneric(
  object,
  def,
  R = 20000L,
  alpha = c(0.001, 0.01, 0.05),
  decomposition = "eigen",
  pd = TRUE,
  tol = 1e-06,
  seed = NULL
)
```

## Arguments

- object:

  R object. Fitted model object with `coef` and `vcov` methods that
  return a named vector of estimated parameters and sampling
  variance-covariance matrix, respectively.

- def:

  List of character strings. A list of defined functions of parameters.
  The string should be a valid R expression when parsed and should
  result a single value when evaluated.

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

  Function used ("MCGeneric").

## Details

A sampling distribution of parameter estimates is generated from the
multivariate normal distribution using the parameter estimates and the
sampling variance-covariance matrix. Confidence intervals for defined
parameters are generated using the simulated sampling distribution.
Parameters are defined using the `def` argument.

## References

MacKinnon, D. P., Lockwood, C. M., & Williams, J. (2004). Confidence
limits for the indirect effect: Distribution of the product and
resampling methods. *Multivariate Behavioral Research*, *39*(1), 99-128.
[doi:10.1207/s15327906mbr3901_4](https://doi.org/10.1207/s15327906mbr3901_4)

Pesigan, I. J. A., & Cheung, S. F. (2024). Monte Carlo confidence
intervals for the indirect effect with missing data. *Behavior Research
Methods*.
[doi:10.3758/s13428-023-02114-4](https://doi.org/10.3758/s13428-023-02114-4)

Preacher, K. J., & Selig, J. P. (2012). Advantages of Monte Carlo
confidence intervals for indirect effects. *Communication Methods and
Measures*, *6*(2), 77–98.
[doi:10.1080/19312458.2012.679848](https://doi.org/10.1080/19312458.2012.679848)

## See also

Other Monte Carlo in Structural Equation Modeling Functions:
[`Func()`](https://github.com/jeksterslab/semmcci/reference/Func.md),
[`MC()`](https://github.com/jeksterslab/semmcci/reference/MC.md),
[`MCFunc()`](https://github.com/jeksterslab/semmcci/reference/MCFunc.md),
[`MCMI()`](https://github.com/jeksterslab/semmcci/reference/MCMI.md),
[`MCStd()`](https://github.com/jeksterslab/semmcci/reference/MCStd.md)

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
library(semmcci)
library(lavaan)

# Data ---------------------------------------------------------------------
data("Tal.Or", package = "psych")
df <- mice::ampute(Tal.Or)$amp

# Monte Carlo --------------------------------------------------------------
## Fit Model in lavaan -----------------------------------------------------
model <- "
  reaction ~ cp * cond + b * pmi
  pmi ~ a * cond
  cond ~~ cond
"
fit <- sem(data = df, model = model, missing = "fiml")

## MCGeneric() -------------------------------------------------------------
MCGeneric(
  fit,
  R = 5L, # use a large value e.g., 20000L for actual research
  alpha = 0.05,
  def = list(
    "a * b",
    "cp + (a * b)"
  )
)
#>                 est     se R   2.5%  97.5%
#> a * b        0.2900 0.1137 5 0.1279 0.3980
#> cp + (a * b) 0.4965 0.1962 5 0.3252 0.8076
```
