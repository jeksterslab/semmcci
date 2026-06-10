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
#> cp                 0.0287 0.0991 5 -0.1848 0.0616
#> b                  0.4144 0.0757 5  0.3450 0.5244
#> a                  0.1591 0.0821 5  0.1081 0.2896
#> cond~~cond         1.0000 0.0000 5  1.0000 1.0000
#> reaction~~reaction 0.8237 0.0590 5  0.7382 0.8643
#> pmi~~pmi           0.9747 0.0333 5  0.9161 0.9880
#> indirect           0.4890 0.0387 5  0.0516 0.1471
#> direct             4.0773 0.0991 5 -0.1848 0.0616
#> total              0.9513 0.1205 5 -0.1332 0.1595

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
#>                       est     se R    2.5%  97.5%
#> cp                 0.0536 0.0773 5 -0.0131 0.1841
#> b                  0.4060 0.0809 5  0.3407 0.5313
#> a                  0.1487 0.0640 5  0.0700 0.2306
#> cond~~cond         1.0000 0.0000 5  1.0000 1.0000
#> reaction~~reaction 0.8258 0.0818 5  0.6921 0.8840
#> pmi~~pmi           0.9779 0.0187 5  0.9466 0.9943
#> indirect           0.0604 0.0358 5  0.0252 0.1094
#> direct             0.0536 0.0773 5 -0.0131 0.1841
#> total              0.1140 0.0860 5  0.0415 0.2531
```
