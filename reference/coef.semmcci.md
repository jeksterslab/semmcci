# Parameter Estimates

Parameter Estimates

## Usage

``` r
# S3 method for class 'semmcci'
coef(object, ...)
```

## Arguments

- object:

  Object of class `semmcci`.

- ...:

  additional arguments.

## Value

Returns a vector of parameter estimates.

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
coef(unstd)
#>                 cp                  b                  a         cond~~cond 
#>          0.2387180          0.4812565          0.5722387          0.2491255 
#> reaction~~reaction           pmi~~pmi         reaction~1              pmi~1 
#>          1.9301333          1.7263351          0.7027880          5.2689670 
#>             cond~1           indirect             direct              total 
#>          0.4852165          0.2753936          0.2387180          0.5141116 
coef(std)
#>                 cp                  b                  a         cond~~cond 
#>         0.07697822         0.41806030         0.21242103         1.00000000 
#> reaction~~reaction           pmi~~pmi         reaction~1              pmi~1 
#>         0.80562787         0.95487731         0.45404397         3.91865219 
#>             cond~1           indirect             direct              total 
#>         0.97213491         0.08880480         0.07697822         0.16578301 

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
coef(unstd)
#>                 cp                  b                  a         cond~~cond 
#>          0.1685092          0.4589709          0.5011024          0.2492432 
#> reaction~~reaction           pmi~~pmi           indirect             direct 
#>          1.9960264          1.7489519          0.2325101          0.1685092 
#>              total 
#>          0.4010193 
coef(std)
#>                 cp                  b                  a         cond~~cond 
#>         0.03267557         0.42341103         0.24847576         1.00000000 
#> reaction~~reaction           pmi~~pmi           indirect             direct 
#>         0.81277998         0.93825980         0.10520738         0.03267557 
#>              total 
#>         0.13788295 
```
