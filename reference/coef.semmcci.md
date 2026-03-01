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
#>          0.2658766          0.4704099          0.3888388          0.2466416 
#> reaction~~reaction           pmi~~pmi         reaction~1              pmi~1 
#>          1.9396195          1.7688235          0.6922417          5.3945599 
#>             cond~1           indirect             direct              total 
#>          0.4485313          0.1829136          0.2658766          0.4487902 
coef(std)
#>                 cp                  b                  a         cond~~cond 
#>         0.08557756         0.40972808         0.14369120         1.00000000 
#> reaction~~reaction           pmi~~pmi         reaction~1              pmi~1 
#>         0.81472274         0.97935284         0.44864665         4.01405539 
#>             cond~1           indirect             direct              total 
#>         0.90314927         0.05887432         0.08557756         0.14445188 

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
#>          0.3183181          0.4912705          0.4057651          0.2465993 
#> reaction~~reaction           pmi~~pmi           indirect             direct 
#>          1.9103791          1.7655638          0.2004329          0.3183181 
#>              total 
#>          0.5187510 
coef(std)
#>                 cp                  b                  a         cond~~cond 
#>         0.08872728         0.43502714         0.15172186         1.00000000 
#> reaction~~reaction           pmi~~pmi           indirect             direct 
#>         0.79116630         0.97698048         0.06600312         0.08872728 
#>              total 
#>         0.15473040 
```
