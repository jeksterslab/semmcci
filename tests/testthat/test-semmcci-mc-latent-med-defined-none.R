## ---- test-semmcci-mc-latent-med-defined-none
lapply(
  X = 1,
  FUN = function(i,
                 R,
                 tol,
                 text) {
    message(text)
    seed <- 42
    data <- lavaan::HolzingerSwineford1939
    # cfa
    model <- "
      visual  =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
      speed   =~ x7 + x8 + x9
    "
    fit <- lavaan::cfa(
      model = model,
      data = data
    )
    set.seed(seed)
    results_chol <- MC(
      fit,
      R = R,
      alpha = c(0.001, 0.01, 0.05),
      decomposition = "chol"
    )
    set.seed(seed)
    results_eigen <- MC(
      fit,
      R = R,
      alpha = c(0.001, 0.01, 0.05),
      decomposition = "eigen"
    )
    set.seed(seed)
    results_svd <- MC(
      fit,
      R = R,
      alpha = c(0.001, 0.01, 0.05),
      decomposition = "svd"
    )
    set.seed(seed)
    answers <- MASS::mvrnorm(
      n = R,
      mu = lavaan::coef(fit),
      Sigma = lavaan::vcov(fit)
    )
    testthat::test_that(
      paste(text, "chol"),
      {
        testthat::expect_equal(
          results_chol$thetahat$est[names(lavaan::coef(fit))],
          as.vector(lavaan::coef(fit)),
          check.attributes = FALSE
        )
        testthat::expect_true(
          abs(
            .MCCI(
              results_chol
            )["visual~~textual", "97.5%"] - quantile(
              answers[, "visual~~textual"],
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
          results_eigen$thetahat$est[names(lavaan::coef(fit))],
          as.vector(lavaan::coef(fit)),
          check.attributes = FALSE
        )
        testthat::expect_true(
          abs(
            .MCCI(
              results_eigen
            )["visual~~textual", "97.5%"] - quantile(
              answers[, "visual~~textual"],
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
          results_svd$thetahat$est[names(lavaan::coef(fit))],
          as.vector(lavaan::coef(fit)),
          check.attributes = FALSE
        )
        testthat::expect_true(
          abs(
            .MCCI(
              results_svd
            )["visual~~textual", "97.5%"] - quantile(
              answers[, "visual~~textual"],
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
  text = "test-semmcci-mc-latent-med-defined-none"
)
