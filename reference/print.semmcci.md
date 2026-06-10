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
#> cp                 0.2643 0.2659 5 -0.2830 -0.2778 -0.2547 0.4199 0.4440 0.4494
#> b                  0.5458 0.0682 5  0.5004  0.5004  0.5006 0.6427 0.6454 0.6460
#> a                  0.4206 0.2024 5  0.0593  0.0636  0.0828 0.5878 0.6005 0.6033
#> cond~~cond         0.2471 0.0333 5  0.1790  0.1795  0.1816 0.2579 0.2583 0.2584
#> reaction~~reaction 1.8502 0.2344 5  1.6840  1.6858  1.6940 2.2641 2.2902 2.2961
#> pmi~~pmi           1.6964 0.1334 5  1.4995  1.5005  1.5045 1.8081 1.8144 1.8158
#> reaction~1         0.3254 0.4596 5 -0.0543 -0.0534 -0.0494 0.8719 0.8747 0.8753
#> pmi~1              5.4071 0.1980 5  5.1954  5.1988  5.2137 5.6948 5.7038 5.7059
#> cond~1             0.4478 0.0514 5  0.3851  0.3864  0.3922 0.5154 0.5166 0.5169
#> indirect           0.2296 0.1086 5  0.0382  0.0402  0.0492 0.2991 0.3015 0.3020
#> direct             0.2643 0.2659 5 -0.2830 -0.2778 -0.2547 0.4199 0.4440 0.4494
#> total              0.4938 0.2840 5  0.0186  0.0190  0.0209 0.6750 0.6977 0.7028
print(std)
#> Standardized Monte Carlo Confidence Intervals
#>                       est     se R   0.05%    0.5%    2.5%  97.5%  99.5% 99.95%
#> cp                 0.0845 0.0832 5 -0.0940 -0.0923 -0.0846 0.1268 0.1341 0.1358
#> b                  0.4632 0.0585 5  0.3830  0.3842  0.3894 0.5337 0.5367 0.5373
#> a                  0.1585 0.0739 5  0.0204  0.0222  0.0303 0.2167 0.2224 0.2237
#> cond~~cond         1.0000 0.0000 5  1.0000  1.0000  1.0000 1.0000 1.0000 1.0000
#> reaction~~reaction 0.7659 0.0604 5  0.7101  0.7102  0.7102 0.8434 0.8465 0.8472
#> pmi~~pmi           0.9749 0.0181 5  0.9500  0.9505  0.9526 0.9982 0.9993 0.9996
#> indirect           0.2093 0.0342 5  0.0109  0.0116  0.0144 0.0979 0.0998 0.1003
#> direct             4.0990 0.0832 5 -0.0940 -0.0923 -0.0846 0.1268 0.1341 0.1358
#> total              0.9009 0.0845 5  0.0062  0.0063  0.0068 0.2025 0.2105 0.2123

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
