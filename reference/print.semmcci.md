# Print Method for Object of Class `semmcci`

Print Method for Object of Class `semmcci`

## Usage

``` r
# S3 method for class 'semmcci'
print(x, alpha = NULL, digits = 4, ...)
```

## Arguments

- x:

  an object of class `semmcci`.

- alpha:

  Numeric vector. Significance level \\\alpha\\. If `alpha = NULL`, use
  the argument `alpha` used in `x`.

- digits:

  Integer indicating the number of decimal places to display.

- ...:

  further arguments.

## Value

Prints a matrix of estimates, standard errors, number of Monte Carlo
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
print(unstd)
#> Monte Carlo Confidence Intervals
#>                       est     se R   0.05%    0.5%    2.5%  97.5%  99.5% 99.95%
#> cp                 0.1593 0.3947 5 -0.4627 -0.4597 -0.4463 0.4056 0.4141 0.4160
#> b                  0.5192 0.0393 5  0.4710  0.4723  0.4780 0.5721 0.5739 0.5743
#> a                  0.4360 0.2370 5  0.2624  0.2672  0.2889 0.8465 0.8498 0.8506
#> cond~~cond         0.2484 0.0102 5  0.2413  0.2415  0.2425 0.2666 0.2668 0.2669
#> reaction~~reaction 1.8979 0.3744 5  1.4834  1.4887  1.5125 2.4561 2.4952 2.5041
#> pmi~~pmi           1.7089 0.2266 5  1.4151  1.4152  1.4154 1.8953 1.9011 1.9024
#> reaction~1         0.4784 0.2342 5  0.0795  0.0805  0.0852 0.5988 0.6059 0.6075
#> pmi~1              5.3712 0.0971 5  5.2141  5.2147  5.2173 5.4487 5.4616 5.4645
#> cond~1             0.4623 0.0504 5  0.3978  0.3995  0.4069 0.5254 0.5267 0.5270
#> indirect           0.2264 0.1141 5  0.1440  0.1469  0.1599 0.4350 0.4381 0.4388
#> direct             0.1593 0.3947 5 -0.4627 -0.4597 -0.4463 0.4056 0.4141 0.4160
#> total              0.3856 0.5007 5 -0.3188 -0.3128 -0.2864 0.8407 0.8522 0.8548
print(std)
#> Standardized Monte Carlo Confidence Intervals
#>                       est     se R   0.05%    0.5%    2.5%  97.5%  99.5% 99.95%
#> cp                 0.0513 0.1305 5 -0.1649 -0.1635 -0.1576 0.1279 0.1299 0.1303
#> b                  0.4446 0.0563 5  0.3904  0.3906  0.3913 0.5212 0.5257 0.5268
#> a                  0.1640 0.0939 5  0.0974  0.0992  0.1074 0.3360 0.3397 0.3405
#> cond~~cond         1.0000 0.0000 5  1.0000  1.0000  1.0000 1.0000 1.0000 1.0000
#> reaction~~reaction 0.7923 0.0490 5  0.7121  0.7124  0.7139 0.8256 0.8278 0.8283
#> pmi~~pmi           0.9731 0.0412 5  0.8841  0.8846  0.8869 0.9875 0.9900 0.9905
#> indirect           0.3091 0.0356 5  0.0513  0.0520  0.0555 0.1373 0.1374 0.1375
#> direct             4.0532 0.1305 5 -0.1649 -0.1635 -0.1576 0.1279 0.1299 0.1303
#> total              0.9275 0.1637 5 -0.1136 -0.1115 -0.1021 0.2652 0.2673 0.2678

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
print(unstd)
#> Monte Carlo Confidence Intervals (Multiple Imputation Estimates)
#>                       est     se R   0.05%    0.5%    2.5%  97.5%  99.5% 99.95%
#> cp                 0.1080 0.2202 5 -0.0399 -0.0330 -0.0021 0.5231 0.5296 0.5310
#> b                  0.5234 0.1310 5  0.3456  0.3469  0.3526 0.6420 0.6433 0.6436
#> a                  0.5281 0.1979 5  0.0325  0.0356  0.0496 0.5223 0.5295 0.5312
#> cond~~cond         0.2481 0.0229 5  0.2117  0.2121  0.2140 0.2685 0.2690 0.2692
#> reaction~~reaction 1.9501 0.2810 5  1.5453  1.5510  1.5762 2.2337 2.2428 2.2448
#> pmi~~pmi           1.7127 0.3034 5  1.3450  1.3492  1.3679 2.0915 2.1030 2.1055
#> indirect           0.2754 0.1174 5  0.0113  0.0135  0.0233 0.3210 0.3311 0.3334
#> direct             0.1080 0.2202 5 -0.0399 -0.0330 -0.0021 0.5231 0.5296 0.5310
#> total              0.3834 0.2505 5  0.1114  0.1177  0.1457 0.7351 0.7381 0.7388
print(std)
#> Standardized Monte Carlo Confidence Intervals
#>                       est     se R   0.05%    0.5%    2.5%  97.5%  99.5% 99.95%
#> cp                 0.0385 0.0691 5 -0.0132 -0.0110 -0.0016 0.1597 0.1606 0.1608
#> b                  0.4652 0.0761 5  0.3307  0.3317  0.3362 0.5243 0.5315 0.5331
#> a                  0.1219 0.0696 5  0.0113  0.0127  0.0191 0.1879 0.1904 0.1909
#> cond~~cond         1.0000 0.0000 5  1.0000  1.0000  1.0000 1.0000 1.0000 1.0000
#> reaction~~reaction 0.7778 0.0737 5  0.6804  0.6823  0.6906 0.8658 0.8670 0.8673
#> pmi~~pmi           0.9851 0.0143 5  0.9636  0.9637  0.9646 0.9991 0.9997 0.9999
#> indirect           0.0567 0.0356 5  0.0038  0.0044  0.0074 0.0980 0.1011 0.1018
#> direct             0.0385 0.0691 5 -0.0132 -0.0110 -0.0016 0.1597 0.1606 0.1608
#> total              0.0952 0.0748 5  0.0367  0.0386  0.0473 0.2228 0.2236 0.2237
```
