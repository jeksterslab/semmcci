## ---- test-semmcci-mc-simple-med-defined-none-mi
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
    model <- "
      y ~ cp * x + b * m
      m ~ a * x
    "
    fit <- lavaan::sem(
      data = data,
      model = model,
      fixed.x = FALSE
    )
    results_mice <- MCMI(
      fit,
      R = R,
      alpha = c(0.001, 0.01, 0.05),
      fun = "mice",
      seed_mc = seed,
      seed_mi = seed,
      m = m
    )
    results_imp <- MCMI(
      fit,
      R = R,
      alpha = c(0.001, 0.01, 0.05),
      seed_mc = seed,
      imp = mice::complete(
        mice::mice(
          data = data,
          print = FALSE,
          m = m,
          seed = seed
        ),
        action = "all"
      )
    )
    set.seed(seed)
    answers <- MASS::mvrnorm(
      n = R,
      mu = lavaan::coef(fit),
      Sigma = lavaan::vcov(fit)
    )
    answers <- cbind(
      answers
    )
    testthat::test_that(
      paste(text, "mice"),
      {
        testthat::expect_equal(
          results_mice$thetahat$est,
          lavaan::parameterEstimates(fit)$est,
          check.attributes = FALSE
        )
        testthat::expect_true(
          abs(
            .MCCI(
              results_mice
            )["cp", "97.5%"] - quantile(
              answers[, "cp"],
              .975,
              na.rm = TRUE
            )
          ) <= tol
        )
      }
    )
    testthat::test_that(
      paste(text, "imp"),
      {
        testthat::expect_equal(
          results_mice,
          results_imp
        )
      }
    )
  },
  n = 1000L,
  R = 2000L,
  m = 5,
  tol = 0.05,
  text = "test-semmcci-mc-simple-med-defined-none-mi"
)
