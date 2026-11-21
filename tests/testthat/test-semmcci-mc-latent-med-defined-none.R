## ---- test-semmcci-mc-latent-med-defined-none
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
        testthat::expect_equal(
          object = results_chol$thetahat$est[
            names(x = lavaan::coef(object = fit))
          ],
          expected = as.vector(x = lavaan::coef(object = fit)),
          check.attributes = FALSE
        )
        testthat::expect_true(
          object = abs(
            x = .MCCI(
              object = results_chol
            )["visual~~textual", "97.5%"] - stats::quantile(
              x = answers[, "visual~~textual"],
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
        testthat::expect_equal(
          object = results_eigen$thetahat$est[
            names(x = lavaan::coef(object = fit))
          ],
          expected = as.vector(x = lavaan::coef(object = fit)),
          check.attributes = FALSE
        )
        testthat::expect_true(
          object = abs(
            x = .MCCI(
              object = results_eigen
            )["visual~~textual", "97.5%"] - stats::quantile(
              x = answers[, "visual~~textual"],
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
        testthat::expect_equal(
          object = results_svd$thetahat$est[
            names(x = lavaan::coef(object = fit))
          ],
          expected = as.vector(x = lavaan::coef(object = fit)),
          check.attributes = FALSE
        )
        testthat::expect_true(
          object = abs(
            x = .MCCI(
              object = results_svd
            )["visual~~textual", "97.5%"] - stats::quantile(
              x = answers[, "visual~~textual"],
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
