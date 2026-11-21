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
#> cp                  0.14744086 0.7424700
#> b                   0.39090238 0.6275561
#> a                   0.25381409 0.9820779
#> cond~~cond          0.17972965 0.2524786
#> reaction~~reaction  1.34975606 2.2492021
#> pmi~~pmi            1.29018870 1.9165897
#> reaction~1         -0.06056016 1.1563401
#> pmi~1               5.03084446 5.5610599
#> cond~1              0.40614694 0.4774201
#> indirect            0.10571693 0.6181069
#> direct              0.14744086 0.7424700
#> total               0.28137648 0.9628970
confint(std)
#>                         2.5 %    97.5 %
#> cp                 0.04537312 0.2215249
#> b                  0.33150232 0.5391845
#> a                  0.08727236 0.3392317
#> cond~~cond         1.00000000 1.0000000
#> reaction~~reaction 0.64807415 0.8778460
#> pmi~~pmi           0.88325550 0.9923690
#> indirect           0.03291200 0.1751983
#> direct             0.04537312 0.2215249
#> total              0.08728519 0.2877579

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
#> cp                 -0.1277621 0.3134395
#> b                   0.4424190 0.5855230
#> a                   0.2282173 0.6821085
#> cond~~cond          0.2131850 0.2870581
#> reaction~~reaction  1.2286826 2.0855107
#> pmi~~pmi            1.3630857 2.1419737
#> indirect            0.1223737 0.3994222
#> direct             -0.1277621 0.3134395
#> total               0.1907682 0.7064029
confint(std)
#>                          2.5 %     97.5 %
#> cp                 -0.03458369 0.09823299
#> b                   0.39858622 0.52586143
#> a                   0.08546547 0.25137873
#> cond~~cond          1.00000000 1.00000000
#> reaction~~reaction  0.71392419 0.83603130
#> pmi~~pmi            0.93663455 0.99182310
#> indirect            0.04385663 0.12294416
#> direct             -0.03458369 0.09823299
#> total               0.05456732 0.22117716
```
