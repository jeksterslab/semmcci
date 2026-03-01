# Monte Carlo Confidence Intervals (Function)

Calculates Monte Carlo confidence intervals for defined parameters.

## Usage

``` r
MCFunc(
  coef,
  vcov,
  func,
  ...,
  R = 20000L,
  alpha = c(0.001, 0.01, 0.05),
  decomposition = "eigen",
  pd = TRUE,
  tol = 1e-06,
  seed = NULL,
  ncores = NULL
)
```

## Arguments

- coef:

  Numeric vector. Vector of estimated parameters.

- vcov:

  Numeric matrix. Sampling variance-covariance matrix of estimated
  parameters.

- func:

  R function.

  1.  The first argument `x` is the argument `coef`.

  2.  The function algebraically manipulates `coef` to return at a new
      numeric vector. It is best to have a named vector as an output.

  3.  The function can take additional named arguments passed using
      `...`.

- ...:

  Additional arguments to pass to `func`.

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

- ncores:

  Positive integer. Number of cores to use. If `ncores = NULL`, use
  single core.

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

  Function used ("MCFunc").

## Details

A sampling distribution of parameter estimates is generated from the
multivariate normal distribution using the parameter estimates and the
sampling variance-covariance matrix. Confidence intervals for defined
parameters are generated using the simulated sampling distribution.
Parameters are defined using the `func` argument.

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
[`MCGeneric()`](https://github.com/jeksterslab/semmcci/reference/MCGeneric.md),
[`MCMI()`](https://github.com/jeksterslab/semmcci/reference/MCMI.md),
[`MCStd()`](https://github.com/jeksterslab/semmcci/reference/MCStd.md)

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
library(semmcci)

## MCFunc() ----------------------------------------------------------------
### Define func ------------------------------------------------------------
func <- function(x) {
  out <- exp(x)
  names(out) <- "exp"
  out
}
### Generate Confidence Intervals ------------------------------------------
MCFunc(
  coef = 0,
  vcov = matrix(1),
  func = func,
  R = 5L, # use a large value e.g., 20000L for actual research
  alpha = 0.05
)
#>     est     se R   2.5% 97.5%
#> exp   1 1.7355 5 0.3318 4.363
```
