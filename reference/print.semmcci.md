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
#> cp                 0.2643 0.2540 5 -0.2660 -0.2610 -0.2387 0.4095 0.4330 0.4383
#> b                  0.5458 0.0681 5  0.5002  0.5003  0.5005 0.6426 0.6453 0.6459
#> a                  0.4206 0.2039 5  0.0615  0.0656  0.0836 0.5924 0.6056 0.6086
#> cond~~cond         0.2471 0.0334 5  0.1791  0.1795  0.1816 0.2581 0.2585 0.2586
#> reaction~~reaction 1.8502 0.2414 5  1.4342  1.4394  1.4623 2.0532 2.0624 2.0644
#> pmi~~pmi           1.6964 0.1318 5  1.5036  1.5048  1.5100 1.8180 1.8259 1.8277
#> reaction~1         0.3254 0.4607 5 -0.0530 -0.0520 -0.0478 0.8678 0.8690 0.8693
#> pmi~1              5.4071 0.2019 5  5.1967  5.1999  5.2143 5.7063 5.7166 5.7189
#> cond~1             0.4478 0.0519 5  0.3846  0.3859  0.3917 0.5159 0.5169 0.5172
#> indirect           0.2296 0.1090 5  0.0396  0.0415  0.0499 0.3013 0.3039 0.3045
#> direct             0.2643 0.2540 5 -0.2660 -0.2610 -0.2387 0.4095 0.4330 0.4383
#> total              0.4938 0.2729 5  0.0381  0.0383  0.0394 0.6627 0.6846 0.6896
print(std)
#> Standardized Monte Carlo Confidence Intervals
#>                       est     se R   0.05%    0.5%    2.5%  97.5%  99.5% 99.95%
#> cp                 0.0845 0.0793 5 -0.0813 -0.0797 -0.0730 0.1290 0.1370 0.1388
#> b                  0.4632 0.0469 5  0.4104  0.4115  0.4162 0.5257 0.5263 0.5264
#> a                  0.1585 0.0747 5  0.0212  0.0229  0.0304 0.2193 0.2253 0.2267
#> cond~~cond         1.0000 0.0000 5  1.0000  1.0000  1.0000 1.0000 1.0000 1.0000
#> reaction~~reaction 0.7659 0.0585 5  0.6816  0.6824  0.6863 0.8336 0.8389 0.8401
#> pmi~~pmi           0.9749 0.0188 5  0.9486  0.9491  0.9514 0.9983 0.9993 0.9995
#> indirect           0.2093 0.0320 5  0.0110  0.0118  0.0152 0.0917 0.0928 0.0930
#> direct             4.0990 0.0793 5 -0.0813 -0.0797 -0.0730 0.1290 0.1370 0.1388
#> total              0.9009 0.0851 5  0.0116  0.0117  0.0119 0.2081 0.2165 0.2183

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
#> cp                 0.2706 0.2655 5  0.0095  0.0132  0.0295 0.6206 0.6217 0.6219
#> b                  0.5224 0.0836 5  0.3956  0.3971  0.4035 0.6054 0.6087 0.6095
#> a                  0.4154 0.2309 5 -0.0809 -0.0744 -0.0457 0.4845 0.4849 0.4850
#> cond~~cond         0.2471 0.0201 5  0.2396  0.2397  0.2402 0.2852 0.2861 0.2863
#> reaction~~reaction 1.9278 0.2548 5  1.4165  1.4214  1.4433 2.0871 2.1063 2.1106
#> pmi~~pmi           1.6481 0.3208 5  1.1216  1.1281  1.1570 1.9128 1.9247 1.9274
#> indirect           0.2176 0.1186 5 -0.0319 -0.0287 -0.0142 0.2681 0.2714 0.2722
#> direct             0.2706 0.2655 5  0.0095  0.0132  0.0295 0.6206 0.6217 0.6219
#> total              0.4882 0.2215 5  0.2401  0.2433  0.2571 0.7662 0.7694 0.7702
print(std)
#> Standardized Monte Carlo Confidence Intervals
#>                       est     se R   0.05%    0.5%    2.5%  97.5%  99.5% 99.95%
#> cp                 0.0609 0.0901 5  0.0032  0.0043  0.0096 0.2073 0.2078 0.2079
#> b                  0.4932 0.0620 5  0.3583  0.3594  0.3642 0.5207 0.5273 0.5288
#> a                  0.1505 0.0958 5 -0.0296 -0.0270 -0.0151 0.2173 0.2203 0.2210
#> cond~~cond         1.0000 0.0000 5  1.0000  1.0000  1.0000 1.0000 1.0000 1.0000
#> reaction~~reaction 0.7440 0.0547 5  0.7068  0.7074  0.7101 0.8340 0.8349 0.8351
#> pmi~~pmi           0.9773 0.0189 5  0.9512  0.9514  0.9526 0.9978 0.9988 0.9991
#> indirect           0.0742 0.0413 5 -0.0106 -0.0095 -0.0047 0.0964 0.0982 0.0985
#> direct             0.0609 0.0901 5  0.0032  0.0043  0.0096 0.2073 0.2078 0.2079
#> total              0.1351 0.0795 5  0.0802  0.0812  0.0854 0.2634 0.2639 0.2641
```
