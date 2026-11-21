## ---- test-semmcci-mc-latent-med-defined
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
        results_chol <- MC(
          lav = fit,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "chol",
          seed = seed
        )
        results_eigen <- MC(
          lav = fit,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "eigen",
          seed = seed
        )
        results_svd <- MC(
          lav = fit,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "svd",
          seed = seed
        )
        set.seed(seed = seed)
        answers <- MASS::mvrnorm(
          n = R,
          mu = lavaan::coef(object = fit),
          Sigma = lavaan::vcov(object = fit)
        )
        answers <- cbind(
          answers,
          ab = answers[, "a"] * answers[, "b"]
        )
        testthat::expect_equal(
          object = results_chol$thetahat$est,
          expected = lavaan::parameterEstimates(object = fit)$est,
          check.attributes = FALSE
        )
        testthat::expect_true(
          object = abs(
            x = .MCCI(
              object = results_chol
            )["ab", "97.5%"] - stats::quantile(
              x = answers[, "ab"],
              .975,
              na.rm = TRUE
            )
          ) <= tol
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
        results_chol <- MC(
          lav = fit,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "chol",
          seed = seed
        )
        results_eigen <- MC(
          lav = fit,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "eigen",
          seed = seed
        )
        results_svd <- MC(
          lav = fit,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "svd",
          seed = seed
        )
        set.seed(seed = seed)
        answers <- MASS::mvrnorm(
          n = R,
          mu = lavaan::coef(object = fit),
          Sigma = lavaan::vcov(object = fit)
        )
        answers <- cbind(
          answers,
          ab = answers[, "a"] * answers[, "b"]
        )
        testthat::expect_equal(
          object = results_eigen$thetahat$est,
          expected = lavaan::parameterEstimates(object = fit)$est,
          check.attributes = FALSE
        )
        testthat::expect_true(
          object = abs(
            x = .MCCI(
              object = results_eigen
            )["ab", "97.5%"] - stats::quantile(
              x = answers[, "ab"],
              .975,
              na.rm = TRUE
            )
          ) <= tol
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
        results_chol <- MC(
          lav = fit,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "chol",
          seed = seed
        )
        results_eigen <- MC(
          lav = fit,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "eigen",
          seed = seed
        )
        results_svd <- MC(
          lav = fit,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "svd",
          seed = seed
        )
        set.seed(seed = seed)
        answers <- MASS::mvrnorm(
          n = R,
          mu = lavaan::coef(object = fit),
          Sigma = lavaan::vcov(object = fit)
        )
        answers <- cbind(
          answers,
          ab = answers[, "a"] * answers[, "b"]
        )
        testthat::expect_equal(
          object = results_svd$thetahat$est,
          expected = lavaan::parameterEstimates(object = fit)$est,
          check.attributes = FALSE
        )
        testthat::expect_true(
          object = abs(
            x = .MCCI(
              object = results_svd
            )["ab", "97.5%"] - stats::quantile(
              x = answers[, "ab"],
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
