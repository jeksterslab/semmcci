## ---- test-semmcci-mc-latent-med-defined
lapply(
  X = 1,
  FUN = function(i,
                 R,
                 tol,
                 text) {
    message(text)
    seed <- 42
    data <- lavaan::HolzingerSwineford1939
    model <- "
      visual  =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
      speed   =~ x7 + x8 + x9
      textual ~ a * visual
      speed ~ b * textual
      ab := a * b
    "
    fit <- lavaan::sem(
      model = model,
      data = data
    )
    run <- TRUE
    tryCatch(
      {
        results_chol <- MC(
          fit,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "chol",
          seed = seed
        )
      },
      error = function() {
        run <- FALSE # nolint
      }
    )
    results_eigen <- MC(
      fit,
      R = R,
      alpha = c(0.001, 0.01, 0.05),
      decomposition = "eigen",
      seed = seed
    )
    results_svd <- MC(
      fit,
      R = R,
      alpha = c(0.001, 0.01, 0.05),
      decomposition = "svd",
      seed = seed
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
      paste(text, "chol"),
      {
        testthat::expect_equal(
          results_chol$thetahat$est,
          lavaan::parameterEstimates(fit)$est,
          check.attributes = FALSE
        )
        testthat::expect_true(
          abs(
            .MCCI(
              results_chol
            )["ab", "97.5%"] - quantile(
              answers[, "ab"],
              .975,
              na.rm = TRUE
            )
          ) <= tol
        )
      }
    )
    testthat::test_that(
      paste(text, "eigen"),
      {
        testthat::expect_equal(
          results_eigen$thetahat$est,
          lavaan::parameterEstimates(fit)$est,
          check.attributes = FALSE
        )
        testthat::expect_true(
          abs(
            .MCCI(
              results_eigen
            )["ab", "97.5%"] - quantile(
              answers[, "ab"],
              .975,
              na.rm = TRUE
            )
          ) <= tol
        )
      }
    )
    testthat::test_that(
      paste(text, "svd"),
      {
        testthat::expect_equal(
          results_svd$thetahat$est,
          lavaan::parameterEstimates(fit)$est,
          check.attributes = FALSE
        )
        testthat::expect_true(
          abs(
            .MCCI(
              results_svd
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
  R = 2000L,
  tol = 0.05,
  text = "test-semmcci-mc-latent-med-defined"
)
