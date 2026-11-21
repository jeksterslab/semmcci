## ---- test-semmcci-mc-simple-med-std-defined
lapply(
  X = 1,
  FUN = function(i,
                 n,
                 R,
                 text) {
    message(text)

    set.seed(seed = 42)

    testthat::skip_on_cran()

    testthat::test_that(
      desc = paste(text, "chol"),
      code = {
        testthat::skip_on_cran()
        seed <- 42
        set.seed(seed = seed)
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
        x <- stats::rnorm(n = n, sd = sqrt(x = sigma2x))
        m <- a * x + stats::rnorm(n = n, sd = sqrt(x = sigma2em))
        y <- cp * x + b * m + stats::rnorm(n = n, sd = sqrt(x = sigma2ey))
        data <- data.frame(x, m, y)
        model <- "
          y ~ cp * x + b * m
          m ~ a * x
          ab := a * b
        "
        fit <- lavaan::sem(
          data = data,
          model = model,
          fixed.x = FALSE
        )
        results_unstd_chol <- MC(
          lav = fit,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "chol",
          seed = seed
        )
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
        estimates <- lavaan::parameterEstimates(object = fit)$est
        results_unstd_svd$thetahatstar[3, ] <- estimates
        results_svd <- MCStd(mc = results_unstd_svd)
        testthat::expect_equal(
          object = results_chol$thetahatstar[3, ],
          expected = lavaan::standardizedSolution(object = fit)$est.std,
          check.attributes = FALSE
        )
        testthat::expect_equal(
          object = .MCCI(
            object = results_chol
          )["ab", "97.5%"],
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
        set.seed(seed = seed)
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
        x <- stats::rnorm(n = n, sd = sqrt(x = sigma2x))
        m <- a * x + stats::rnorm(n = n, sd = sqrt(x = sigma2em))
        y <- cp * x + b * m + stats::rnorm(n = n, sd = sqrt(x = sigma2ey))
        data <- data.frame(x, m, y)
        model <- "
          y ~ cp * x + b * m
          m ~ a * x
          ab := a * b
        "
        fit <- lavaan::sem(
          data = data,
          model = model,
          fixed.x = FALSE
        )
        results_unstd_chol <- MC(
          lav = fit,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "chol",
          seed = seed
        )
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
        estimates <- lavaan::parameterEstimates(object = fit)$est
        results_unstd_svd$thetahatstar[3, ] <- estimates
        results_svd <- MCStd(mc = results_unstd_svd)
        testthat::expect_equal(
          object = results_eigen$thetahatstar[3, ],
          expected = lavaan::standardizedSolution(object = fit)$est.std,
          check.attributes = FALSE
        )
        testthat::expect_equal(
          object = .MCCI(
            object = results_eigen
          )["ab", "97.5%"],
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
        set.seed(seed = seed)
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
        x <- stats::rnorm(n = n, sd = sqrt(x = sigma2x))
        m <- a * x + stats::rnorm(n = n, sd = sqrt(x = sigma2em))
        y <- cp * x + b * m + stats::rnorm(n = n, sd = sqrt(x = sigma2ey))
        data <- data.frame(x, m, y)
        model <- "
          y ~ cp * x + b * m
          m ~ a * x
          ab := a * b
        "
        fit <- lavaan::sem(
          data = data,
          model = model,
          fixed.x = FALSE
        )
        results_unstd_chol <- MC(
          lav = fit,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "chol",
          seed = seed
        )
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
        estimates <- lavaan::parameterEstimates(object = fit)$est
        results_unstd_svd$thetahatstar[3, ] <- estimates
        results_svd <- MCStd(mc = results_unstd_svd)
        testthat::expect_equal(
          object = results_svd$thetahatstar[3, ],
          expected = lavaan::standardizedSolution(object = fit)$est.std,
          check.attributes = FALSE
        )
        testthat::expect_equal(
          object = .MCCI(
            object = results_svd
          )["ab", "97.5%"],
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
  n = 1000L,
  R = 2000L,
  text = "test-semmcci-mc-simple-med-std-defined"
)
