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
#>                       est     se R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%
#> cp                 0.3992 0.2061 5 0.1500 0.1502 0.1512 0.5686 0.5689 0.5690
#> b                  0.5120 0.0748 5 0.4092 0.4094 0.4100 0.5632 0.5642 0.5644
#> a                  0.5764 0.2205 5 0.4136 0.4163 0.4280 0.9527 0.9668 0.9700
#> cond~~cond         0.2496 0.0289 5 0.2273 0.2277 0.2293 0.2992 0.3007 0.3011
#> reaction~~reaction 1.9422 0.3038 5 1.5006 1.5031 1.5142 2.2388 2.2634 2.2689
#> pmi~~pmi           1.7423 0.0742 5 1.8380 1.8396 1.8469 2.0352 2.0413 2.0426
#> reaction~1         0.4668 0.3236 5 0.3228 0.3260 0.3405 1.0954 1.1112 1.1148
#> pmi~1              5.3099 0.1170 5 5.0949 5.0952 5.0962 5.3526 5.3584 5.3597
#> cond~1             0.4929 0.0288 5 0.4549 0.4553 0.4572 0.5275 0.5291 0.5294
#> indirect           0.2951 0.1275 5 0.2177 0.2179 0.2188 0.5150 0.5313 0.5349
#> direct             0.3992 0.2061 5 0.1500 0.1502 0.1512 0.5686 0.5689 0.5690
#> total              0.6944 0.2397 5 0.3802 0.3818 0.3888 0.8968 0.8999 0.9006
summary(std)
#> Standardized Monte Carlo Confidence Intervals
#>                       est     se R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%
#> cp                 0.1257 0.0762 5 0.0464 0.0465 0.0470 0.2044 0.2052 0.2054
#> b                  0.4358 0.0527 5 0.3897 0.3901 0.3919 0.5060 0.5064 0.5065
#> a                  0.2131 0.0777 5 0.1392 0.1404 0.1455 0.3310 0.3345 0.3352
#> cond~~cond         1.0000 0.0000 5 1.0000 1.0000 1.0000 1.0000 1.0000 1.0000
#> reaction~~reaction 0.7709 0.0294 5 0.6976 0.6983 0.7016 0.7699 0.7703 0.7704
#> pmi~~pmi           0.9546 0.0375 5 0.8876 0.8881 0.8903 0.9785 0.9802 0.9806
#> indirect           0.2941 0.0388 5 0.0697 0.0700 0.0711 0.1642 0.1687 0.1698
#> direct             3.9304 0.0762 5 0.0464 0.0465 0.0470 0.2044 0.2052 0.2054
#> total              0.9866 0.0873 5 0.1217 0.1221 0.1240 0.3067 0.3083 0.3087

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
#> cp                 0.4425 0.3137 5 -0.1079 -0.1024 -0.0780 0.7189 0.7491 0.7559
#> b                  0.4900 0.1186 5  0.3351  0.3371  0.3462 0.6223 0.6239 0.6243
#> a                  0.5306 0.2507 5  0.1450  0.1509  0.1774 0.7809 0.7874 0.7888
#> cond~~cond         0.2498 0.0187 5  0.2274  0.2275  0.2279 0.2695 0.2703 0.2705
#> reaction~~reaction 1.9688 0.2009 5  1.8590  1.8590  1.8591 2.2633 2.2657 2.2662
#> pmi~~pmi           1.7175 0.3344 5  1.4483  1.4495  1.4547 2.1547 2.1561 2.1564
#> indirect           0.2611 0.1523 5  0.0649  0.0681  0.0819 0.4636 0.4738 0.4761
#> direct             0.4425 0.3137 5 -0.1079 -0.1024 -0.0780 0.7189 0.7491 0.7559
#> total              0.7036 0.2681 5  0.3680  0.3687  0.3720 0.9674 0.9887 0.9934
summary(std)
#> Standardized Monte Carlo Confidence Intervals
#>                       est     se R   0.05%    0.5%    2.5%  97.5%  99.5% 99.95%
#> cp                 0.1027 0.0948 5 -0.0309 -0.0292 -0.0215 0.2201 0.2293 0.2313
#> b                  0.4442 0.1074 5  0.2628  0.2652  0.2759 0.5450 0.5523 0.5540
#> a                  0.2048 0.0986 5  0.0473  0.0496  0.0599 0.2931 0.2946 0.2949
#> cond~~cond         1.0000 0.0000 5  1.0000  1.0000  1.0000 1.0000 1.0000 1.0000
#> reaction~~reaction 0.7735 0.0750 5  0.6543  0.6566  0.6666 0.8424 0.8435 0.8438
#> pmi~~pmi           0.9581 0.0347 5  0.9130  0.9132  0.9140 0.9949 0.9972 0.9977
#> indirect           0.0910 0.0440 5  0.0187  0.0197  0.0240 0.1337 0.1360 0.1365
#> direct             0.1027 0.0948 5 -0.0309 -0.0292 -0.0215 0.2201 0.2293 0.2313
#> total              0.1936 0.0838 5  0.1055  0.1057  0.1067 0.2962 0.3026 0.3040
```
