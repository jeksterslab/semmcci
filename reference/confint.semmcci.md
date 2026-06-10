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
#> cp                 -0.12780127 0.7154221
#> b                   0.37955340 0.6583398
#> a                   0.13774531 0.6855574
#> cond~~cond          0.18027195 0.2530756
#> reaction~~reaction  1.37210278 2.2875221
#> pmi~~pmi            1.28474910 1.9140500
#> reaction~1         -0.06058291 1.1563873
#> pmi~1               5.15664442 5.5024511
#> cond~1              0.40930467 0.4742592
#> indirect            0.06173600 0.4053126
#> direct             -0.12780127 0.7154221
#> total               0.25865179 0.9358804
confint(std)
#>                          2.5 %    97.5 %
#> cp                 -0.03480193 0.2143712
#> b                   0.31330302 0.5510251
#> a                   0.04740425 0.2583324
#> cond~~cond          1.00000000 1.0000000
#> reaction~~reaction  0.66982595 0.8447119
#> pmi~~pmi            0.93315615 0.9963912
#> indirect            0.01850981 0.1160392
#> direct             -0.03480193 0.2143712
#> total               0.07381830 0.2805111

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
#> cp                 -0.14995801 0.4109224
#> b                   0.44634838 0.6073850
#> a                  -0.12663687 0.7490378
#> cond~~cond          0.21245147 0.2869141
#> reaction~~reaction  1.70126346 2.2532347
#> pmi~~pmi            1.63113261 2.0323056
#> indirect           -0.07290374 0.4550059
#> direct             -0.14995801 0.4109224
#> total               0.25450317 0.6908409
confint(std)
#>                          2.5 %    97.5 %
#> cp                 -0.04435560 0.1136109
#> b                   0.39462158 0.5548349
#> a                  -0.04176436 0.2736435
#> cond~~cond          1.00000000 1.0000000
#> reaction~~reaction  0.69848874 0.8389369
#> pmi~~pmi            0.92487793 0.9958554
#> indirect           -0.01983783 0.1410071
#> direct             -0.04435560 0.1136109
#> total               0.07658417 0.2139349
```
