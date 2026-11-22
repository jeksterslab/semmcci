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

  Numeric vector. Significance level \\(\alpha\\). If `alpha = NULL`,
  use the argument `alpha` used in `x`.

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
#> cp                 0.2643 0.2655 5 -0.2845 -0.2793 -0.2561 0.4179 0.4414 0.4467
#> b                  0.5458 0.0672 5  0.5010  0.5011  0.5014 0.6425 0.6452 0.6458
#> a                  0.4206 0.2483 5  0.0709  0.0715  0.0740 0.6056 0.6107 0.6119
#> cond~~cond         0.2471 0.0335 5  0.1788  0.1793  0.1814 0.2582 0.2586 0.2587
#> reaction~~reaction 1.8502 0.2334 5  1.6833  1.6851  1.6934 2.2617 2.2878 2.2936
#> pmi~~pmi           1.6964 0.1338 5  1.4976  1.4985  1.5029 1.8091 1.8155 1.8169
#> reaction~1         0.3254 0.4593 5 -0.0538 -0.0529 -0.0490 0.8717 0.8745 0.8751
#> pmi~1              5.4071 0.1390 5  5.2793  5.2804  5.2853 5.6046 5.6133 5.6153
#> cond~1             0.4478 0.0525 5  0.3863  0.3875  0.3929 0.5177 0.5187 0.5189
#> indirect           0.2296 0.1423 5  0.0458  0.0459  0.0463 0.3638 0.3717 0.3735
#> direct             0.2643 0.2655 5 -0.2845 -0.2793 -0.2561 0.4179 0.4414 0.4467
#> total              0.4938 0.2903 5 -0.0101 -0.0090 -0.0041 0.6309 0.6402 0.6422
print(std)
#> Standardized Monte Carlo Confidence Intervals
#>                       est     se R   0.05%    0.5%    2.5%  97.5%  99.5% 99.95%
#> cp                 0.0845 0.0832 5 -0.0945 -0.0928 -0.0851 0.1264 0.1338 0.1354
#> b                  0.4632 0.0586 5  0.3834  0.3846  0.3896 0.5337 0.5366 0.5372
#> a                  0.1585 0.0865 5  0.0244  0.0247  0.0261 0.2066 0.2068 0.2068
#> cond~~cond         1.0000 0.0000 5  1.0000  1.0000  1.0000 1.0000 1.0000 1.0000
#> reaction~~reaction 0.7659 0.0604 5  0.7113  0.7113  0.7116 0.8458 0.8491 0.8498
#> pmi~~pmi           0.9749 0.0208 5  0.9572  0.9572  0.9573 0.9993 0.9994 0.9994
#> indirect           0.2093 0.0408 5  0.0131  0.0131  0.0134 0.0993 0.1000 0.1001
#> direct             4.0990 0.0832 5 -0.0945 -0.0928 -0.0851 0.1264 0.1338 0.1354
#> total              0.9009 0.0846 5 -0.0034 -0.0030 -0.0016 0.1894 0.1937 0.1947

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
#> cp                 0.2706 0.2655 5  0.0096  0.0133  0.0296 0.6207 0.6217 0.6220
#> b                  0.5224 0.0836 5  0.3956  0.3971  0.4035 0.6054 0.6087 0.6095
#> a                  0.4154 0.2309 5 -0.0809 -0.0744 -0.0457 0.4843 0.4848 0.4849
#> cond~~cond         0.2471 0.0211 5  0.2068  0.2070  0.2082 0.2567 0.2574 0.2576
#> reaction~~reaction 1.9278 0.2548 5  1.4165  1.4214  1.4433 2.0870 2.1063 2.1106
#> pmi~~pmi           1.6481 0.3208 5  1.1216  1.1281  1.1570 1.9128 1.9247 1.9274
#> indirect           0.2176 0.1185 5 -0.0319 -0.0286 -0.0142 0.2681 0.2714 0.2722
#> direct             0.2706 0.2655 5  0.0096  0.0133  0.0296 0.6207 0.6217 0.6220
#> total              0.4882 0.2215 5  0.2402  0.2433  0.2572 0.7662 0.7695 0.7702
print(std)
#> Standardized Monte Carlo Confidence Intervals
#>                       est     se R   0.05%    0.5%    2.5%  97.5%  99.5% 99.95%
#> cp                 0.0609 0.0876 5  0.0027  0.0039  0.0092 0.2072 0.2089 0.2093
#> b                  0.4932 0.0621 5  0.3578  0.3589  0.3637 0.5207 0.5273 0.5288
#> a                  0.1505 0.0920 5 -0.0307 -0.0283 -0.0173 0.2122 0.2171 0.2182
#> cond~~cond         1.0000 0.0000 5  1.0000  1.0000  1.0000 1.0000 1.0000 1.0000
#> reaction~~reaction 0.7440 0.0538 5  0.7066  0.7073  0.7102 0.8321 0.8327 0.8328
#> pmi~~pmi           0.9773 0.0177 5  0.9524  0.9528  0.9546 0.9980 0.9988 0.9990
#> indirect           0.0742 0.0401 5 -0.0110 -0.0100 -0.0055 0.0942 0.0968 0.0973
#> direct             0.0609 0.0876 5  0.0027  0.0039  0.0092 0.2072 0.2089 0.2093
#> total              0.1351 0.0780 5  0.0683  0.0695  0.0747 0.2581 0.2603 0.2609
```
