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
  [`MC()`](https://jeksterslab.github.io/semmcci/reference/MC.md) or
  [`MCMI()`](https://jeksterslab.github.io/semmcci/reference/MCMI.md)
  function.

- alpha:

  Numeric vector. Significance level \\(\alpha\\).

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
[`Func()`](https://jeksterslab.github.io/semmcci/reference/Func.md),
[`MC()`](https://jeksterslab.github.io/semmcci/reference/MC.md),
[`MCFunc()`](https://jeksterslab.github.io/semmcci/reference/MCFunc.md),
[`MCGeneric()`](https://jeksterslab.github.io/semmcci/reference/MCGeneric.md),
[`MCMI()`](https://jeksterslab.github.io/semmcci/reference/MCMI.md)

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
#> cp                 0.0287 0.1234 5 -0.1719 0.1354
#> b                  0.4144 0.0557 5  0.3699 0.5088
#> a                  0.1591 0.0353 5  0.0117 0.1011
#> cond~~cond         1.0000 0.0000 5  1.0000 1.0000
#> reaction~~reaction 0.8237 0.0405 5  0.7394 0.8350
#> pmi~~pmi           0.9747 0.0042 5  0.9897 0.9998
#> indirect           0.4890 0.0145 5  0.0056 0.0424
#> direct             4.0773 0.1234 5 -0.1719 0.1354
#> total              0.9513 0.1269 5 -0.1542 0.1620

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
#> cp                 0.0536 0.0698 5 -0.0046 0.1708
#> b                  0.4060 0.1083 5  0.2485 0.5078
#> a                  0.1487 0.0563 5  0.0919 0.2196
#> cond~~cond         1.0000 0.0000 5  1.0000 1.0000
#> reaction~~reaction 0.8258 0.0732 5  0.7407 0.9198
#> pmi~~pmi           0.9779 0.0175 5  0.9518 0.9913
#> indirect           0.0604 0.0186 5  0.0448 0.0858
#> direct             0.0536 0.0698 5 -0.0046 0.1708
#> total              0.1140 0.0784 5  0.0413 0.2398
```
