## ---- test-semmcci-mc-latent-med-std-defined-none
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
      textual ~ visual
      speed ~ textual
    "
    fit <- lavaan::sem(
      model = model,
      data = data
    )
    run <- TRUE
    tryCatch(
      {
        results_unstd_chol <- MC(
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
    # insert original estimate on the third row
    results_unstd_chol$thetahatstar[3, ] <- lavaan::parameterEstimates(fit)$est
    results_chol <- MCStd(results_unstd_chol)
    results_unstd_eigen <- MC(
      fit,
      R = R,
      alpha = c(0.001, 0.01, 0.05),
      decomposition = "eigen",
      seed = seed
    )
    # insert original estimate on the third row
    results_unstd_eigen$thetahatstar[3, ] <- lavaan::parameterEstimates(fit)$est
    results_eigen <- MCStd(results_unstd_eigen)
    results_unstd_svd <- MC(
      fit,
      R = R,
      alpha = c(0.001, 0.01, 0.05),
      decomposition = "svd",
      seed = seed
    )
    # insert original estimate on the third row
    results_unstd_svd$thetahatstar[3, ] <- lavaan::parameterEstimates(fit)$est
    results_svd <- MCStd(results_unstd_svd)
    if (run) {
      testthat::test_that(
        paste(text, "chol"),
        {
          testthat::expect_equal(
            results_chol$thetahatstar[3, ],
            lavaan::standardizedSolution(fit)$est.std,
            check.attributes = FALSE
          )
          testthat::expect_equal(
            .MCCI(
              results_chol
            )["textual~visual", "97.5%"],
            quantile(
              results_chol$thetahatstar[, "textual~visual"],
              .975,
              na.rm = TRUE
            ),
            check.attributes = FALSE
          )
        }
      )
    }
    testthat::test_that(
      paste(text, "eigen"),
      {
        testthat::expect_equal(
          results_eigen$thetahatstar[3, ],
          lavaan::standardizedSolution(fit)$est.std,
          check.attributes = FALSE
        )
        testthat::expect_equal(
          .MCCI(
            results_eigen
          )["textual~visual", "97.5%"],
          quantile(
            results_eigen$thetahatstar[, "textual~visual"],
            .975,
            na.rm = TRUE
          ),
          check.attributes = FALSE
        )
      }
    )
    testthat::test_that(
      paste(text, "svd"),
      {
        testthat::expect_equal(
          results_svd$thetahatstar[3, ],
          lavaan::standardizedSolution(fit)$est.std,
          check.attributes = FALSE
        )
        testthat::expect_equal(
          .MCCI(
            results_svd
          )["textual~visual", "97.5%"],
          quantile(
            results_svd$thetahatstar[, "textual~visual"],
            .975,
            na.rm = TRUE
          ),
          check.attributes = FALSE
        )
      }
    )
  },
  R = 2000L,
  tol = 0.05,
  text = "test-semmcci-mc-latent-med-std-defined-none"
)
