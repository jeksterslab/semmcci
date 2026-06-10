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
#> cp                  0.14744403 0.7424726
#> b                   0.39090186 0.6275559
#> a                   0.25381433 0.9820780
#> cond~~cond          0.17972964 0.2524786
#> reaction~~reaction  1.34975722 2.2492023
#> pmi~~pmi            1.29018738 1.9165882
#> reaction~1         -0.06055857 1.1563419
#> pmi~1               5.03084449 5.5610594
#> cond~1              0.40614698 0.4774202
#> indirect            0.10571688 0.6181068
#> direct              0.14744403 0.7424726
#> total               0.28138005 0.9628996
confint(std)
#>                         2.5 %    97.5 %
#> cp                 0.04537408 0.2215256
#> b                  0.33150148 0.5391836
#> a                  0.08727263 0.3392317
#> cond~~cond         1.00000000 1.0000000
#> reaction~~reaction 0.64807453 0.8778464
#> pmi~~pmi           0.88325556 0.9923690
#> indirect           0.03291196 0.1751982
#> direct             0.04537408 0.2215256
#> total              0.08728626 0.2877587

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
#>                         2.5 %    97.5 %
#> cp                 -0.1466898 0.7886030
#> b                   0.4348012 0.6264601
#> a                   0.4356127 0.7189728
#> cond~~cond          0.2125691 0.2869361
#> reaction~~reaction  1.6957537 2.3318747
#> pmi~~pmi            1.6706384 2.0537057
#> indirect            0.2495174 0.4143079
#> direct             -0.1466898 0.7886030
#> total               0.2661524 1.0697778
confint(std)
#>                          2.5 %    97.5 %
#> cp                 -0.04356042 0.2062070
#> b                   0.38845923 0.5553418
#> a                   0.16823396 0.2456598
#> cond~~cond          1.00000000 1.0000000
#> reaction~~reaction  0.70036018 0.8317012
#> pmi~~pmi            0.93961792 0.9716926
#> indirect            0.07097573 0.1246613
#> direct             -0.04356042 0.2062070
#> total               0.08010079 0.2799355
```
