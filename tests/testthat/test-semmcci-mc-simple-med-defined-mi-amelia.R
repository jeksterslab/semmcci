## ---- test-semmcci-mc-simple-med-defined-mi-amelia
lapply(
  X = 1,
  FUN = function(i,
                 n,
                 R,
                 m,
                 tol,
                 text) {
    message(text)
    seed <- 42
    set.seed(seed)
    cp <- 0.00
    b <- 0.10
    a <- 0.10
    sigma2ey <- 1 - b^2 - cp^2 - 2 * a * b * cp
    sigma2em <- 1 - a^2
    sigma2x <- 1
    x <- rnorm(n = n, sd = sqrt(sigma2x))
    m <- a * x + rnorm(n = n, sd = sqrt(sigma2em))
    y <- cp * x + b * m + rnorm(n = n, sd = sqrt(sigma2ey))
    data <- data.frame(x, m, y)
    data[1, "x"] <- NA
    data[2, "m"] <- NA
    data[3, "y"] <- NA
    model <- "
      y ~ cp * x + b * m
      m ~ a * x
      ab := a * b
    "
    fit <- lavaan::sem(
      data = data,
      model = model,
      fixed.x = FALSE,
      missing = "fiml"
    )
    results_amelia <- MCMI(
      fit,
      R = R,
      alpha = c(0.001, 0.01, 0.05),
      decomposition = "eigen",
      seed = seed,
      mi = Amelia::amelia(
        x = data,
        p2s = 0,
        m = 5L
      )
    )
    set.seed(seed)
    answers <- MASS::mvrnorm(
      n = R,
      mu = lavaan::coef(fit),
      Sigma = lavaan::vcov(fit)
    )
    answers <- cbind(
      answers,
      ab = answers[, "a"] * answers[, "b"]
    )
    testthat::test_that(
      paste(text, "mice"),
      {
        testthat::expect_equal(
          results_amelia$thetahat$est,
          lavaan::parameterEstimates(fit)$est,
          check.attributes = FALSE,
          tolerance = tol
        )
        testthat::expect_true(
          abs(
            .MCCI(
              results_amelia
            )["ab", "97.5%"] - quantile(
              answers[, "ab"],
              .975,
              na.rm = TRUE
            )
          ) <= tol
        )
      }
    )
  },
  n = 1000L,
  R = 2000L,
  m = 5,
  tol = 0.05,
  text = "test-semmcci-mc-simple-med-defined-mi-amelia"
)
