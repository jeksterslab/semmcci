## ---- test-semmcci-mc-simple-med-defined-inequality
lapply(
  X = 1,
  FUN = function(i,
                 n,
                 R,
                 tol,
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
        x <- rnorm(n = n, sd = sqrt(sigma2x))
        m <- a * x + rnorm(n = n, sd = sqrt(sigma2em))
        y <- cp * x + b * m + rnorm(n = n, sd = sqrt(sigma2ey))
        data <- data.frame(x, m, y)
        model <- "
          y ~ cp * x + b * m
          m ~ a * x
          ab := a * b
          ab > 0
        "
        fit <- lavaan::sem(
          data = data,
          model = model,
          fixed.x = FALSE
        )
        results_chol <- MC(
          fit,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "chol",
          seed = seed
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
        testthat::expect_equal(
          results_chol$thetahat$est[c(1:6, 8)],
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
        testthat::skip_on_cran()
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
          ab := a * b
          ab > 0
        "
        fit <- lavaan::sem(
          data = data,
          model = model,
          fixed.x = FALSE
        )
        results_chol <- MC(
          fit,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "chol",
          seed = seed
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
        testthat::expect_equal(
          results_eigen$thetahat$est[c(1:6, 8)],
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
        testthat::skip_on_cran()
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
          ab := a * b
          ab > 0
        "
        fit <- lavaan::sem(
          data = data,
          model = model,
          fixed.x = FALSE
        )
        results_chol <- MC(
          fit,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "chol",
          seed = seed
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
        testthat::expect_equal(
          results_svd$thetahat$est[c(1:6, 8)],
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
  n = 1000L,
  R = 2000L,
  tol = 0.05,
  text = "test-semmcci-mc-simple-med-defined-inequality"
)
