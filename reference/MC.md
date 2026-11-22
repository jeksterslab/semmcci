# Monte Carlo Confidence Intervals

Calculates Monte Carlo confidence intervals for free and defined
parameters.

## Usage

``` r
MC(
  lav,
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

  Function used ("MC").

## Details

A sampling distribution of parameter estimates is generated from the
multivariate normal distribution using the parameter estimates and the
sampling variance-covariance matrix. Confidence intervals for free and
defined parameters are generated using the simulated sampling
distribution. Parameters can be defined using the `:=` operator in the
`lavaan` model syntax.

Let \\(\hat{\boldsymbol{\theta}}\\) be the vector of parameter estimates
and \\(\hat{\boldsymbol{V}}\\) its sampling covariance matrix,
`lavaan::coef(lav)` and `lavaan::vcov(lav)`. The Monte Carlo draws are
\$\$\hat{\boldsymbol{\theta}}^{\ast}\_{r} = \hat{\boldsymbol{\theta}} +
\boldsymbol{A}^{\prime} \boldsymbol{z}\_{r}, \quad \boldsymbol{z}\_{r}
\sim \mathcal{N} \left( \boldsymbol{0}, \boldsymbol{I} \right), \quad r
= 1, \dots, R,\$\$ where \\(\boldsymbol{A}^{\prime} \boldsymbol{A} =
\hat{\boldsymbol{V}}\\). With `decomposition = "eigen"`,
\\(\hat{\boldsymbol{V}} = \boldsymbol{Q} \boldsymbol{\Lambda}
\boldsymbol{Q}^{\prime}\\) and \\(\boldsymbol{A} =
\boldsymbol{\Lambda}\_{+}^{1 / 2} \boldsymbol{Q}^{\prime}\\), with
negative eigenvalues set to zero; with `decomposition = "chol"`,
\\(\boldsymbol{A}\\) is the upper Cholesky factor (the eigenvalue
decomposition is used if it fails); `decomposition = "svd"` uses the
singular value decomposition. With `pd = TRUE`, the function stops
unless every eigenvalue \\(\lambda\_{l} \geq - \mathrm{tol} \left\|
\lambda\_{1} \right\|\\). A defined parameter \\(\phi = h \left(
\boldsymbol{\theta} \right)\\), given by the `:=` operator in the model
syntax, has the draws \\(\hat{\phi}^{\ast}\_{r} = h \left(
\hat{\boldsymbol{\theta}}^{\ast}\_{r} \right)\\). The confidence
intervals are the percentile intervals \\(\left\[ \hat{G}^{-1} \left(
\alpha / 2 \right), \hat{G}^{-1} \left( 1 - \alpha / 2 \right)
\right\]\\), where \\(\hat{G}\\) is the empirical distribution function
of the draws (Preacher & Selig, 2012).

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
[`Func()`](https://jeksterslab.github.io/semmcci/reference/Func.md),
[`MCFunc()`](https://jeksterslab.github.io/semmcci/reference/MCFunc.md),
[`MCGeneric()`](https://jeksterslab.github.io/semmcci/reference/MCGeneric.md),
[`MCMI()`](https://jeksterslab.github.io/semmcci/reference/MCMI.md),
[`MCStd()`](https://jeksterslab.github.io/semmcci/reference/MCStd.md)

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
library(semmcci)
library(lavaan)
#> This is lavaan 0.7-3
#> lavaan is FREE software! Please report any bugs.

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
MC(
  fit,
  R = 5L, # use a large value e.g., 20000L for actual research
  alpha = 0.05
)
#> Monte Carlo Confidence Intervals
#>                       est     se R    2.5%  97.5%
#> cp                 0.3796 0.2694 5  0.0822 0.6708
#> b                  0.4880 0.0883 5  0.4753 0.6730
#> a                  0.5426 0.3561 5 -0.0027 0.8971
#> cond~~cond         0.2496 0.0122 5  0.2267 0.2556
#> reaction~~reaction 1.7760 0.1978 5  1.4029 1.8400
#> pmi~~pmi           1.7238 0.2483 5  1.4317 2.0548
#> reaction~1         0.5868 0.4778 5 -0.3957 0.6505
#> pmi~1              5.3616 0.1768 5  5.1389 5.5827
#> cond~1             0.4939 0.0401 5  0.4377 0.5288
#> indirect           0.2648 0.1951 5 -0.0091 0.4851
#> direct             0.3796 0.2694 5  0.0822 0.6708
#> total              0.6444 0.2664 5  0.2699 0.8628
```
