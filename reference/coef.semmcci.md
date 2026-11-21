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
#>          0.2387183          0.4812564          0.5722394          0.2491255 
#> reaction~~reaction           pmi~~pmi         reaction~1              pmi~1 
#>          1.9301332          1.7263352          0.7027880          5.2689667 
#>             cond~1           indirect             direct              total 
#>          0.4852165          0.2753939          0.2387183          0.5141122 
coef(std)
#>                 cp                  b                  a         cond~~cond 
#>         0.07697832         0.41806031         0.21242125         1.00000000 
#> reaction~~reaction           pmi~~pmi         reaction~1              pmi~1 
#>         0.80562781         0.95487721         0.45404395         3.91865159 
#>             cond~1           indirect             direct              total 
#>         0.97213494         0.08880489         0.07697832         0.16578321 

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
