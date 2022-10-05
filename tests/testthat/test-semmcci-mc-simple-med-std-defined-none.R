## ---- test-semmcci-mc-simple-med-std-defined-none
lapply(
  X = 1,
  FUN = function(i,
                 n,
                 R,
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
    coefs <- c(
      cp = cp,
      b = b,
      a = a,
      ab = a * b
    )
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
    set.seed(seed)
    results_unstd_null <- MC(
      fit,
      R = R,
      alpha = c(0.001, 0.01, 0.05),
      decomposition = NULL
    )
    results_unstd_null$thetahatstar[3, ] <- lavaan::parameterEstimates(fit)$est
    results_null <- MCStd(results_unstd_null)
    set.seed(seed)
    results_unstd_chol <- MC(
      fit,
      R = R,
      alpha = c(0.001, 0.01, 0.05),
      decomposition = "chol"
    )
    results_unstd_chol$thetahatstar[3, ] <- lavaan::parameterEstimates(fit)$est
    results_chol <- MCStd(results_unstd_chol)
    set.seed(seed)
    results_unstd_eigen <- MC(
      fit,
      R = R,
      alpha = c(0.001, 0.01, 0.05),
      decomposition = "eigen"
    )
    results_unstd_eigen$thetahatstar[3, ] <- lavaan::parameterEstimates(fit)$est
    results_eigen <- MCStd(results_unstd_eigen)
    set.seed(seed)
    results_unstd_svd <- MC(
      fit,
      R = R,
      alpha = c(0.001, 0.01, 0.05),
      decomposition = "svd"
    )
    results_unstd_svd$thetahatstar[3, ] <- lavaan::parameterEstimates(fit)$est
    results_svd <- MCStd(results_unstd_svd)
    testthat::test_that(
      paste(text, "NULL"),
      {
        testthat::expect_equal(
          results_null$thetahat$est,
          lavaan::parameterEstimates(fit)$est,
          check.attributes = FALSE
        )
        testthat::expect_equal(
          results_null$thetahatstar_std[3, ],
          lavaan::standardizedSolution(fit)$est.std,
          check.attributes = FALSE
        )
        testthat::expect_equal(
          .MCCI(results_null)["cp", "97.5%"],
          quantile(results_null$thetahatstar_std[, "cp"], .975, na.rm = TRUE),
          check.attributes = FALSE
        )
      }
    )
    testthat::test_that(
      paste(text, "chol"),
      {
        testthat::expect_equal(
          results_chol$thetahat$est,
          lavaan::parameterEstimates(fit)$est,
          check.attributes = FALSE
        )
        testthat::expect_equal(
          results_chol$thetahatstar_std[3, ],
          lavaan::standardizedSolution(fit)$est.std,
          check.attributes = FALSE
        )
        testthat::expect_equal(
          .MCCI(results_chol)["cp", "97.5%"],
          quantile(results_chol$thetahatstar_std[, "cp"], .975, na.rm = TRUE),
          check.attributes = FALSE
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
        testthat::expect_equal(
          results_eigen$thetahatstar_std[3, ],
          lavaan::standardizedSolution(fit)$est.std,
          check.attributes = FALSE
        )
        testthat::expect_equal(
          .MCCI(results_eigen)["cp", "97.5%"],
          quantile(results_eigen$thetahatstar_std[, "cp"], .975, na.rm = TRUE),
          check.attributes = FALSE
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
        testthat::expect_equal(
          results_svd$thetahatstar_std[3, ],
          lavaan::standardizedSolution(fit)$est.std,
          check.attributes = FALSE
        )
        testthat::expect_equal(
          .MCCI(results_svd)["cp", "97.5%"],
          quantile(results_svd$thetahatstar_std[, "cp"], .975, na.rm = TRUE),
          check.attributes = FALSE
        )
      }
    )
  },
  n = 1000L,
  R = 2000L,
  text = "test-semmcci-mc-simple-med-std-defined-none"
)
