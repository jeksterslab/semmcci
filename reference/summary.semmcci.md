# Summary Method for an Object of Class `semmcci`

Summary Method for an Object of Class `semmcci`

## Usage

``` r
# S3 method for class 'semmcci'
summary(object, alpha = NULL, digits = 4, ...)
```

## Arguments

- object:

  Object of class `semmcci`.

- alpha:

  Numeric vector. Significance level \\\alpha\\. If `alpha = NULL`, use
  the argument `alpha` used in `object`.

- digits:

  Digits to print.

- ...:

  additional arguments.

## Value

Returns a matrix of estimates, standard errors, number of Monte Carlo
replications, and confidence intervals.

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
summary(unstd)
#> Monte Carlo Confidence Intervals
#>                       est     se R   0.05%    0.5%    2.5%  97.5%  99.5% 99.95%
#> cp                 0.2758 0.2309 5  0.0487  0.0509  0.0606 0.5772 0.5833 0.5847
#> b                  0.5077 0.0907 5  0.3787  0.3793  0.3823 0.5844 0.5875 0.5882
#> a                  0.3362 0.3414 5 -0.1192 -0.1139 -0.0904 0.6700 0.6714 0.6717
#> cond~~cond         0.2448 0.0209 5  0.2044  0.2045  0.2048 0.2537 0.2552 0.2556
#> reaction~~reaction 1.8981 0.2880 5  1.5563  1.5594  1.5731 2.2139 2.2189 2.2200
#> pmi~~pmi           1.7406 0.3256 5  1.2691  1.2791  1.3235 2.1329 2.1532 2.1578
#> reaction~1         0.4989 0.6719 5 -0.1871 -0.1841 -0.1709 1.3508 1.4010 1.4123
#> pmi~1              5.4719 0.1467 5  5.3094  5.3111  5.3189 5.6405 5.6426 5.6431
#> cond~1             0.4334 0.0355 5  0.4021  0.4023  0.4031 0.4803 0.4810 0.4812
#> indirect           0.1707 0.1733 5 -0.0450 -0.0424 -0.0306 0.3666 0.3682 0.3685
#> direct             0.2758 0.2309 5  0.0487  0.0509  0.0606 0.5772 0.5833 0.5847
#> total              0.4465 0.3187 5  0.0042  0.0139  0.0567 0.7893 0.7959 0.7974
summary(std)
#> Standardized Monte Carlo Confidence Intervals
#>                       est     se R   0.05%    0.5%    2.5%  97.5%  99.5% 99.95%
#> cp                 0.0882 0.0692 5  0.0144  0.0150  0.0181 0.1674 0.1678 0.1679
#> b                  0.4362 0.0991 5  0.2756  0.2777  0.2867 0.5153 0.5160 0.5162
#> a                  0.1251 0.1204 5 -0.0483 -0.0464 -0.0381 0.2318 0.2328 0.2331
#> cond~~cond         1.0000 0.0000 5  1.0000  1.0000  1.0000 1.0000 1.0000 1.0000
#> reaction~~reaction 0.7924 0.0879 5  0.7067  0.7069  0.7079 0.9117 0.9218 0.9241
#> pmi~~pmi           0.9844 0.0244 5  0.9457  0.9458  0.9463 0.9976 0.9976 0.9976
#> indirect           0.3224 0.0530 5 -0.0133 -0.0125 -0.0092 0.1124 0.1131 0.1133
#> direct             4.1150 0.0692 5  0.0144  0.0150  0.0181 0.1674 0.1678 0.1679
#> total              0.8761 0.0975 5  0.0013  0.0042  0.0174 0.2382 0.2384 0.2384

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
summary(unstd)
#> Monte Carlo Confidence Intervals (Multiple Imputation Estimates)
#>                       est     se R   0.05%    0.5%    2.5%  97.5%  99.5% 99.95%
#> cp                 0.2422 0.2586 5 -0.3059 -0.2992 -0.2695 0.3441 0.3490 0.3501
#> b                  0.5185 0.0917 5  0.3837  0.3862  0.3975 0.6289 0.6361 0.6377
#> a                  0.2893 0.2791 5  0.0083  0.0126  0.0316 0.6607 0.6675 0.6691
#> cond~~cond         0.2447 0.0374 5  0.1625  0.1636  0.1685 0.2543 0.2545 0.2546
#> reaction~~reaction 1.8824 0.3055 5  1.5428  1.5518  1.5918 2.2847 2.2883 2.2892
#> pmi~~pmi           1.7001 0.1989 5  1.5726  1.5783  1.6036 2.0662 2.0678 2.0681
#> indirect           0.1493 0.1411 5  0.0052  0.0075  0.0178 0.3473 0.3497 0.3503
#> direct             0.2422 0.2586 5 -0.3059 -0.2992 -0.2695 0.3441 0.3490 0.3501
#> total              0.3914 0.1993 5  0.0137  0.0186  0.0402 0.5067 0.5087 0.5091
summary(std)
#> Standardized Monte Carlo Confidence Intervals
#>                       est     se R   0.05%    0.5%    2.5%  97.5%  99.5% 99.95%
#> cp                 0.0430 0.0748 5 -0.0849 -0.0830 -0.0746 0.1049 0.1068 0.1073
#> b                  0.4251 0.0536 5  0.3659  0.3673  0.3734 0.5043 0.5068 0.5073
#> a                  0.0947 0.0915 5  0.0030  0.0045  0.0112 0.2239 0.2270 0.2277
#> cond~~cond         1.0000 0.0000 5  1.0000  1.0000  1.0000 1.0000 1.0000 1.0000
#> reaction~~reaction 0.8140 0.0493 5  0.7353  0.7356  0.7370 0.8563 0.8619 0.8632
#> pmi~~pmi           0.9910 0.0214 5  0.9482  0.9485  0.9497 0.9993 0.9998 1.0000
#> indirect           0.0403 0.0397 5  0.0015  0.0022  0.0054 0.0995 0.1004 0.1006
#> direct             0.0430 0.0748 5 -0.0849 -0.0830 -0.0746 0.1049 0.1068 0.1073
#> total              0.0833 0.0591 5  0.0038  0.0053  0.0118 0.1481 0.1482 0.1482
```
