## ---- test-semmcci-mc

## With Defined Parameters

# Generate Data ------------------------------------------------------------
n <- 1000
x <- rnorm(n = n)
m <- 0.50 * x + rnorm(n = n)
y <- 0.25 * x + 0.50 * m + rnorm(n = n)
data <- data.frame(x, m, y)

# Fit Model in lavaan ------------------------------------------------------
model <- "
  y ~ cp * x + b * m
  m ~ a * x
  x ~~ var_x * x
  m ~~ var_em * m
  y ~~ var_ey * y
  ab := a * b
"
fit <- lavaan::sem(
  data = data,
  model = model,
  fixed.x = FALSE
)

set.seed(42)
thetahatstar <- MASS::mvrnorm(
  n = 100L,
  mu = lavaan::coef(fit),
  Sigma = lavaan::vcov(fit)
)

coefs <- lavaan::coef(fit)

var_x <- coefs["var_x"]
var_m <- coefs["var_em"] + coefs["var_x"] * coefs["a"]^2
var_y <- coefs["var_ey"] + coefs["var_em"] * coefs["b"]^2 + coefs["var_x"] * (coefs["cp"] + coefs["b"] * coefs["a"])^2

var_x_mc <- thetahatstar[, "var_x"]
var_m_mc <- thetahatstar[, "var_em"] + thetahatstar[, "var_x"] * thetahatstar[, "a"]^2
var_y_mc <- thetahatstar[, "var_ey"] + thetahatstar[, "var_em"] * thetahatstar[, "b"]^2 + thetahatstar[, "var_x"] * (thetahatstar[, "cp"] + thetahatstar[, "b"] * thetahatstar[, "a"])^2



cp_mc <- (thetahatstar[, "cp"]) * sqrt(
  var_x_mc / var_y_mc
)
b_mc <- (thetahatstar[, "b"]) * sqrt(
  var_m_mc / var_y_mc
)
a_mc <- (thetahatstar[, "a"]) * sqrt(
  var_x_mc / var_m_mc
)
ab_mc <- (thetahatstar[, "a"] * thetahatstar[, "b"]) * sqrt(
  var_x_mc / var_y_mc
)


cp <- .pcci(
  cp_mc,
  thetahat = (coefs["cp"]) * sqrt(var_x / var_y),
  alpha = c(0.001, 0.01, 0.05)
)
b <- .pcci(
  b_mc,
  thetahat = (coefs["b"]) * sqrt(var_m / var_y),
  alpha = c(0.001, 0.01, 0.05)
)
a <- .pcci(
  a_mc,
  thetahat = (coefs["a"]) * sqrt(var_x / var_m),
  alpha = c(0.001, 0.01, 0.05)
)
ab <- .pcci(
  ab_mc,
  thetahat = (coefs["a"] * coefs["b"]) * sqrt(var_x / var_y),
  alpha = c(0.001, 0.01, 0.05)
)

print(
  rbind(
    cp,
    b,
    a,
    ab
  )
)
set.seed(42)
# Monte Carlo --------------------------------------------------------------
output <- mc(
  fit,
  R = 100L, # use a large value e.g., 20000L for actual research
  alpha = c(0.001, 0.01, 0.05)
)
print(output)

# Standardized Monte Carlo -------------------------------------------------
output_std <- mc_std(output)
print(output_std)

## Without Defined Parameters

# Fit Model in lavaan ------------------------------------------------------
model <- "
  y ~ cp * x + b * m
  m ~ a * x
"
fit <- lavaan::sem(
  data = data,
  model = model
)

# Monte Carlo --------------------------------------------------------------
output <- mc(
  fit,
  R = 100L, # use a large value e.g., 20000L for actual research
  alpha = c(0.001, 0.01, 0.05)
)
print(output)

# Standardized Monte Carlo -------------------------------------------------
output_std <- mc_std(output)
print(output_std)

testthat::test_that("TRUE", {
  testthat::expect_true(
    TRUE
  )
})

# TODO: Write unit tests
