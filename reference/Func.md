# Monte Carlo Confidence Intervals (List)

Calculates Monte Carlo confidence intervals for defined parameters.

## Usage

``` r
Func(coef, func, ..., est, alpha = c(0.001, 0.01, 0.05), ncores = NULL)
```

## Arguments

- coef:

  List. A list of parameters.

- func:

  R function.

  1.  The first argument `x` is the argument `coef`.

  2.  The function algebraically manipulates `coef` to return at a new
      numeric vector. It is best to have a named vector as an output.

  3.  The function can take additional named arguments passed using
      `...`.

- ...:

  Additional arguments to pass to `func`.

- est:

  Numeric vector. Vector of original parameter estimates.

- alpha:

  Numeric vector. Significance level \\\alpha\\.

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

  Function used ("Func").

## Details

The distribution of parameters is provided as a list (`params`) and the
definition of the function of paremeters is provided by a function
(`func`). Confidence intervals for defined parameters are generated
using the generated sampling distribution.

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
[`MC()`](https://github.com/jeksterslab/semmcci/reference/MC.md),
[`MCFunc()`](https://github.com/jeksterslab/semmcci/reference/MCFunc.md),
[`MCGeneric()`](https://github.com/jeksterslab/semmcci/reference/MCGeneric.md),
[`MCMI()`](https://github.com/jeksterslab/semmcci/reference/MCMI.md),
[`MCStd()`](https://github.com/jeksterslab/semmcci/reference/MCStd.md)

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
library(semmcci)

## Generate Parameters -----------------------------------------------------
coef <- lapply(
  X = 1:5,
  FUN = function(i) {
    rnorm(n = 1)
  }
)

## Func() ------------------------------------------------------------------
### Define func ------------------------------------------------------------
func <- function(x) {
  out <- exp(x)
  names(out) <- "exp"
  out
}
### Generate Confidence Intervals ------------------------------------------
Func(
  coef,
  func = func,
  est = 1,
  alpha = 0.05
)
#>        est     se R   2.5% 97.5%
#> exp 2.7183 1.1354 5 0.1781 3.024
```
