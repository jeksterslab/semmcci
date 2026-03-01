# Monte Carlo Confidence Intervals for the Parameter Estimates

Monte Carlo Confidence Intervals for the Parameter Estimates

## Usage

``` r
# S3 method for class 'semmcci'
confint(object, parm = NULL, level = 0.95, ...)
```

## Arguments

- object:

  Object of class `semmcci`.

- parm:

  a specification of which parameters are to be given confidence
  intervals, either a vector of numbers or a vector of names. If
  missing, all parameters are considered.

- level:

  the confidence level required.

- ...:

  additional arguments.

## Value

Returns a matrix of confidence intervals.

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
  R = 5L # use a large value e.g., 20000L for actual research
)

## Standardized Monte Carlo ------------------------------------------------
std <- MCStd(unstd)
confint(unstd)
#>                          2.5 %    97.5 %
#> cp                 -0.51305096 0.7221367
#> b                   0.37497783 0.6515359
#> a                  -0.06106141 0.6431062
#> cond~~cond          0.21366079 0.2875252
#> reaction~~reaction  1.52601582 2.1859216
#> pmi~~pmi            1.45811700 2.0986159
#> reaction~1         -0.15255498 1.2609993
#> pmi~1               5.18729326 5.5421076
#> cond~1              0.43124384 0.4853911
#> indirect           -0.01373886 0.3726966
#> direct             -0.51305096 0.7221367
#> total              -0.28948942 0.9666925
confint(std)
#>                          2.5 %    97.5 %
#> cp                 -0.14997971 0.2139835
#> b                   0.34233130 0.5610291
#> a                  -0.01867223 0.2340757
#> cond~~cond          1.00000000 1.0000000
#> reaction~~reaction  0.69396208 0.8768936
#> pmi~~pmi            0.94518068 0.9969824
#> indirect           -0.00472690 0.1232964
#> direct             -0.14997971 0.2139835
#> total              -0.08370106 0.2869990

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
  R = 5L # use a large value e.g., 20000L for actual research
)

## Standardized Monte Carlo ------------------------------------------------
std <- MCStd(unstd)
confint(unstd)
#>                          2.5 %    97.5 %
#> cp                 -0.26432646 0.2489080
#> b                   0.47649611 0.6634891
#> a                   0.11764681 1.0199450
#> cond~~cond          0.23962952 0.3093232
#> reaction~~reaction  1.45947586 2.2969139
#> pmi~~pmi            1.73179180 2.1542937
#> indirect            0.07341572 0.6397607
#> direct             -0.26432646 0.2489080
#> total              -0.19091074 0.8376247
confint(std)
#>                          2.5 %     97.5 %
#> cp                 -0.08728326 0.08358825
#> b                   0.47808643 0.54844592
#> a                   0.04627865 0.35690462
#> cond~~cond          1.00000000 1.00000000
#> reaction~~reaction  0.68090331 0.77127817
#> pmi~~pmi            0.86692286 0.99733038
#> indirect            0.02459122 0.19290309
#> direct             -0.08728326 0.08358825
#> total              -0.06269204 0.25331112
```
