## ---- test-semmcci-mc-latent-med-std-defined
lapply(
  X = 1,
  FUN = function(i,
                 R,
                 tol,
                 text) {
    message(text)

    set.seed(seed = 42)

    testthat::skip_on_cran()

    testthat::test_that(
      desc = paste(text, "chol"),
      code = {
        testthat::skip_on_cran()
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
        results_unstd_chol <- MC(
          lav = fit,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "chol",
          seed = seed
        )
        # insert original estimate on the third row
        estimates <- lavaan::parameterEstimates(object = fit)$est
        results_unstd_chol$thetahatstar[3, ] <- estimates
        results_chol <- MCStd(mc = results_unstd_chol)
        results_unstd_eigen <- MC(
          lav = fit,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "eigen",
          seed = seed
        )
        # insert original estimate on the third row
        estimates <- lavaan::parameterEstimates(object = fit)$est
        results_unstd_eigen$thetahatstar[3, ] <- estimates
        results_eigen <- MCStd(mc = results_unstd_eigen)
        results_unstd_svd <- MC(
          lav = fit,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "svd",
          seed = seed
        )
        # insert original estimate on the third row
        estimates <- lavaan::parameterEstimates(object = fit)$est
        results_unstd_svd$thetahatstar[3, ] <- estimates
        results_svd <- MCStd(mc = results_unstd_svd)
        testthat::expect_equal(
          object = results_chol$thetahatstar[3, ],
          expected = lavaan::standardizedSolution(object = fit)$est.std,
          check.attributes = FALSE
        )
        testthat::expect_equal(
          object = .MCCI(object = results_chol)["ab", "97.5%"],
          expected = stats::quantile(
            x = results_chol$thetahatstar[, "ab"],
            .975,
            na.rm = TRUE
          ),
          check.attributes = FALSE
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "eigen"),
      code = {
        testthat::skip_on_cran()
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
        results_unstd_chol <- MC(
          lav = fit,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "chol",
          seed = seed
        )
        # insert original estimate on the third row
        estimates <- lavaan::parameterEstimates(object = fit)$est
        results_unstd_chol$thetahatstar[3, ] <- estimates
        results_chol <- MCStd(mc = results_unstd_chol)
        results_unstd_eigen <- MC(
          lav = fit,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "eigen",
          seed = seed
        )
        # insert original estimate on the third row
        estimates <- lavaan::parameterEstimates(object = fit)$est
        results_unstd_eigen$thetahatstar[3, ] <- estimates
        results_eigen <- MCStd(mc = results_unstd_eigen)
        results_unstd_svd <- MC(
          lav = fit,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "svd",
          seed = seed
        )
        # insert original estimate on the third row
        estimates <- lavaan::parameterEstimates(object = fit)$est
        results_unstd_svd$thetahatstar[3, ] <- estimates
        results_svd <- MCStd(mc = results_unstd_svd)
        testthat::expect_equal(
          object = results_eigen$thetahatstar[3, ],
          expected = lavaan::standardizedSolution(object = fit)$est.std,
          check.attributes = FALSE
        )
        testthat::expect_equal(
          object = .MCCI(object = results_eigen)["ab", "97.5%"],
          expected = stats::quantile(
            x = results_eigen$thetahatstar[, "ab"],
            .975,
            na.rm = TRUE
          ),
          check.attributes = FALSE
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "svd"),
      code = {
        testthat::skip_on_cran()
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
        results_unstd_chol <- MC(
          lav = fit,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "chol",
          seed = seed
        )
        # insert original estimate on the third row
        estimates <- lavaan::parameterEstimates(object = fit)$est
        results_unstd_chol$thetahatstar[3, ] <- estimates
        results_chol <- MCStd(mc = results_unstd_chol)
        results_unstd_eigen <- MC(
          lav = fit,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "eigen",
          seed = seed
        )
        # insert original estimate on the third row
        estimates <- lavaan::parameterEstimates(object = fit)$est
        results_unstd_eigen$thetahatstar[3, ] <- estimates
        results_eigen <- MCStd(mc = results_unstd_eigen)
        results_unstd_svd <- MC(
          lav = fit,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "svd",
          seed = seed
        )
        # insert original estimate on the third row
        estimates <- lavaan::parameterEstimates(object = fit)$est
        results_unstd_svd$thetahatstar[3, ] <- estimates
        results_svd <- MCStd(mc = results_unstd_svd)
        testthat::expect_equal(
          object = results_svd$thetahatstar[3, ],
          expected = lavaan::standardizedSolution(object = fit)$est.std,
          check.attributes = FALSE
        )
        testthat::expect_equal(
          object = .MCCI(object = results_svd)["ab", "97.5%"],
          expected = stats::quantile(
            x = results_svd$thetahatstar[, "ab"],
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
  text = "test-semmcci-mc-latent-med-std-defined"
)
