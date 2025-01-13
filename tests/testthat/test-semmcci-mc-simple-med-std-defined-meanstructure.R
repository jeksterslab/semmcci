## ---- test-semmcci-mc-simple-med-std-defined-meanstructure
lapply(
  X = 1,
  FUN = function(i,
                 n,
                 R,
                 text) {
    message(text)
    testthat::test_that(
      paste(text, "chol"),
      {
        testthat::skip_on_cran()
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
          ab := a * b
        "
        fit <- lavaan::sem(
          data = data,
          model = model,
          fixed.x = FALSE,
          meanstructure = TRUE
        )
        results_unstd_chol <- MC(
          fit,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "chol",
          seed = seed
        )
        estimates <- lavaan::parameterEstimates(fit)$est
        results_unstd_chol$thetahatstar[3, ] <- estimates
        results_chol <- MCStd(results_unstd_chol)
        results_unstd_eigen <- MC(
          fit,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "eigen",
          seed = seed
        )
        estimates <- lavaan::parameterEstimates(fit)$est
        results_unstd_eigen$thetahatstar[3, ] <- estimates
        results_eigen <- MCStd(results_unstd_eigen)
        results_unstd_svd <- MC(
          fit,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "svd",
          seed = seed
        )
        estimates <- lavaan::parameterEstimates(fit)$est
        results_unstd_svd$thetahatstar[3, ] <- estimates
        results_svd <- MCStd(results_unstd_svd)
        testthat::expect_equal(
          .MCCI(
            results_chol
          )["ab", "97.5%"],
          quantile(
            results_chol$thetahatstar[, "ab"],
            .975,
            na.rm = TRUE
          ),
          check.attributes = FALSE
        )
      }
    )
    testthat::test_that(
      paste(text, "eigen"),
      {
        testthat::skip_on_cran()
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
          ab := a * b
        "
        fit <- lavaan::sem(
          data = data,
          model = model,
          fixed.x = FALSE,
          meanstructure = TRUE
        )
        results_unstd_chol <- MC(
          fit,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "chol",
          seed = seed
        )
        estimates <- lavaan::parameterEstimates(fit)$est
        results_unstd_chol$thetahatstar[3, ] <- estimates
        results_chol <- MCStd(results_unstd_chol)
        results_unstd_eigen <- MC(
          fit,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "eigen",
          seed = seed
        )
        estimates <- lavaan::parameterEstimates(fit)$est
        results_unstd_eigen$thetahatstar[3, ] <- estimates
        results_eigen <- MCStd(results_unstd_eigen)
        results_unstd_svd <- MC(
          fit,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "svd",
          seed = seed
        )
        estimates <- lavaan::parameterEstimates(fit)$est
        results_unstd_svd$thetahatstar[3, ] <- estimates
        results_svd <- MCStd(results_unstd_svd)
        testthat::expect_equal(
          .MCCI(
            results_eigen
          )["ab", "97.5%"],
          quantile(
            results_eigen$thetahatstar[, "ab"],
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
        testthat::skip_on_cran()
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
          ab := a * b
        "
        fit <- lavaan::sem(
          data = data,
          model = model,
          fixed.x = FALSE,
          meanstructure = TRUE
        )
        results_unstd_chol <- MC(
          fit,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "chol",
          seed = seed
        )
        estimates <- lavaan::parameterEstimates(fit)$est
        results_unstd_chol$thetahatstar[3, ] <- estimates
        results_chol <- MCStd(results_unstd_chol)
        results_unstd_eigen <- MC(
          fit,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "eigen",
          seed = seed
        )
        estimates <- lavaan::parameterEstimates(fit)$est
        results_unstd_eigen$thetahatstar[3, ] <- estimates
        results_eigen <- MCStd(results_unstd_eigen)
        results_unstd_svd <- MC(
          fit,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "svd",
          seed = seed
        )
        estimates <- lavaan::parameterEstimates(fit)$est
        results_unstd_svd$thetahatstar[3, ] <- estimates
        results_svd <- MCStd(results_unstd_svd)
        testthat::expect_equal(
          .MCCI(
            results_svd
          )["ab", "97.5%"],
          quantile(
            results_svd$thetahatstar[, "ab"],
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
  text = "test-semmcci-mc-simple-med-std-defined-meanstructure"
)
