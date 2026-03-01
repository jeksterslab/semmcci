# Standardized Monte Carlo Confidence Intervals

Calculates standardized Monte Carlo confidence intervals for free and
defined parameters.

## Usage

``` r
MCStd(mc, alpha = c(0.001, 0.01, 0.05))
```

## Arguments

- mc:

  Output of the
  [`MC()`](https://github.com/jeksterslab/semmcci/reference/MC.md) or
  [`MCMI()`](https://github.com/jeksterslab/semmcci/reference/MCMI.md)
  function.

- alpha:

  Numeric vector. Significance level \\\alpha\\.

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

  Function used ("MCStd").

## Details

The empirical sampling distribution of parameter estimates from the
argument `mc` is standardized, that is, each randomly generated vector
of parameters is standardized. Defined parameters are computed from the
standardized component parameters. Confidence intervals are generated
using the standardized empirical sampling distribution.

## References

Pesigan, I. J. A., & Cheung, S. F. (2024). Monte Carlo confidence
intervals for the indirect effect with missing data. *Behavior Research
Methods*.
[doi:10.3758/s13428-023-02114-4](https://doi.org/10.3758/s13428-023-02114-4)

## See also

Other Monte Carlo in Structural Equation Modeling Functions:
[`Func()`](https://github.com/jeksterslab/semmcci/reference/Func.md),
[`MC()`](https://github.com/jeksterslab/semmcci/reference/MC.md),
[`MCFunc()`](https://github.com/jeksterslab/semmcci/reference/MCFunc.md),
[`MCGeneric()`](https://github.com/jeksterslab/semmcci/reference/MCGeneric.md),
[`MCMI()`](https://github.com/jeksterslab/semmcci/reference/MCMI.md)

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
  indirect := a * b
  direct := cp
  total := cp + (a * b)
"
fit <- sem(data = df, model = model, missing = "fiml")

## MC() --------------------------------------------------------------------
unstd <- MC(
  fit,
  R = 5L, # use a large value e.g., 20000L for actual research
  alpha = 0.05
)

## Standardized Monte Carlo ------------------------------------------------
MCStd(unstd, alpha = 0.05)
#> Standardized Monte Carlo Confidence Intervals
#>                       est     se R    2.5%  97.5%
#> cp                 0.0997 0.1246 5 -0.0433 0.2264
#> b                  0.4399 0.0336 5  0.4471 0.5201
#> a                  0.1703 0.1256 5 -0.0017 0.2840
#> cond~~cond         1.0000 0.0000 5  1.0000 1.0000
#> reaction~~reaction 0.7816 0.0498 5  0.6430 0.7556
#> pmi~~pmi           0.9710 0.0377 5  0.9180 0.9999
#> indirect           0.3622 0.0594 5 -0.0010 0.1320
#> direct             4.0501 0.1246 5 -0.0433 0.2264
#> total              0.9292 0.1721 5 -0.0349 0.3402

# Monte Carlo (Multiple Imputation) ----------------------------------------
## Multiple Imputation -----------------------------------------------------
mi <- mice::mice(
  data = df,
  print = FALSE,
  m = 5L, # use a large value e.g., 100L for actual research,
  seed = 42
)

## Fit Model in lavaan -----------------------------------------------------
fit <- sem(data = df, model = model) # use default listwise deletion

## MCMI() ------------------------------------------------------------------
unstd <- MCMI(
  fit,
  mi = mi,
  R = 5L, # use a large value e.g., 20000L for actual research
  alpha = 0.05
)

## Standardized Monte Carlo ------------------------------------------------
MCStd(unstd, alpha = 0.05)
#> Standardized Monte Carlo Confidence Intervals
#>                       est     se R   2.5%  97.5%
#> cp                 0.1350 0.1145 5 0.0605 0.2908
#> b                  0.4360 0.0659 5 0.4069 0.5655
#> a                  0.1463 0.1037 5 0.0890 0.3335
#> cond~~cond         1.0000 0.0000 5 1.0000 1.0000
#> reaction~~reaction 0.7744 0.0829 5 0.6166 0.8122
#> pmi~~pmi           0.9786 0.0446 5 0.8886 0.9918
#> indirect           0.0638 0.0486 5 0.0488 0.1578
#> direct             0.1350 0.1145 5 0.0605 0.2908
#> total              0.1988 0.1412 5 0.1304 0.4295
```
