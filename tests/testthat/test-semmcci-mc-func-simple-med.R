## ---- test-semmcci-mc-func-simple-med
lapply(
  X = 1,
  FUN = function(i,
                 n,
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
        func <- function(x) {
          ab <- x[2] * x[3]
          names(x = ab) <- "ab"
          ab
        }
        ## MCFunc
        results_chol <- MCFunc(
          coef = lavaan::coef(object = fit),
          vcov = lavaan::vcov(object = fit),
          func = func,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "chol",
          seed = seed
        )
        results_eigen <- MCFunc(
          coef = lavaan::coef(object = fit),
          vcov = lavaan::vcov(object = fit),
          func = func,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "eigen",
          seed = seed
        )
        results_svd <- MCFunc(
          coef = lavaan::coef(object = fit),
          vcov = lavaan::vcov(object = fit),
          func = func,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "svd",
          seed = seed
        )
        set.seed(seed = seed)
        coefs <- MASS::mvrnorm(
          n = R,
          mu = lavaan::coef(object = fit),
          Sigma = lavaan::vcov(object = fit)
        )
        answers <- cbind(
          coefs,
          ab = coefs[, "a"] * coefs[, "b"]
        )
        ## Func
        coef <- as.data.frame(
          x = t(x = coefs)
        )
        results_func <- Func(
          coef = coef,
          func = func,
          est = lavaan::coef(object = fit),
          alpha = c(0.001, 0.01, 0.05)
        )
        testthat::expect_equal(
          object = results_chol$thetahat$est,
          expected = lavaan::parameterEstimates(object = fit)[7, "est"],
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
        func <- function(x) {
          ab <- x[2] * x[3]
          names(x = ab) <- "ab"
          ab
        }
        ## MCFunc
        results_chol <- MCFunc(
          coef = lavaan::coef(object = fit),
          vcov = lavaan::vcov(object = fit),
          func = func,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "chol",
          seed = seed
        )
        results_eigen <- MCFunc(
          coef = lavaan::coef(object = fit),
          vcov = lavaan::vcov(object = fit),
          func = func,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "eigen",
          seed = seed
        )
        results_svd <- MCFunc(
          coef = lavaan::coef(object = fit),
          vcov = lavaan::vcov(object = fit),
          func = func,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "svd",
          seed = seed
        )
        set.seed(seed = seed)
        coefs <- MASS::mvrnorm(
          n = R,
          mu = lavaan::coef(object = fit),
          Sigma = lavaan::vcov(object = fit)
        )
        answers <- cbind(
          coefs,
          ab = coefs[, "a"] * coefs[, "b"]
        )
        ## Func
        coef <- as.data.frame(
          x = t(x = coefs)
        )
        results_func <- Func(
          coef = coef,
          func = func,
          est = lavaan::coef(object = fit),
          alpha = c(0.001, 0.01, 0.05)
        )
        testthat::expect_equal(
          object = results_eigen$thetahat$est,
          expected = lavaan::parameterEstimates(object = fit)[7, "est"],
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
        func <- function(x) {
          ab <- x[2] * x[3]
          names(x = ab) <- "ab"
          ab
        }
        ## MCFunc
        results_chol <- MCFunc(
          coef = lavaan::coef(object = fit),
          vcov = lavaan::vcov(object = fit),
          func = func,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "chol",
          seed = seed
        )
        results_eigen <- MCFunc(
          coef = lavaan::coef(object = fit),
          vcov = lavaan::vcov(object = fit),
          func = func,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "eigen",
          seed = seed
        )
        results_svd <- MCFunc(
          coef = lavaan::coef(object = fit),
          vcov = lavaan::vcov(object = fit),
          func = func,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "svd",
          seed = seed
        )
        set.seed(seed = seed)
        coefs <- MASS::mvrnorm(
          n = R,
          mu = lavaan::coef(object = fit),
          Sigma = lavaan::vcov(object = fit)
        )
        answers <- cbind(
          coefs,
          ab = coefs[, "a"] * coefs[, "b"]
        )
        ## Func
        coef <- as.data.frame(
          x = t(x = coefs)
        )
        results_func <- Func(
          coef = coef,
          func = func,
          est = lavaan::coef(object = fit),
          alpha = c(0.001, 0.01, 0.05)
        )
        testthat::expect_equal(
          object = results_svd$thetahat$est,
          expected = lavaan::parameterEstimates(object = fit)[7, "est"],
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
    testthat::test_that(
      desc = paste(text, "func"),
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
        func <- function(x) {
          ab <- x[2] * x[3]
          names(x = ab) <- "ab"
          ab
        }
        ## MCFunc
        results_chol <- MCFunc(
          coef = lavaan::coef(object = fit),
          vcov = lavaan::vcov(object = fit),
          func = func,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "chol",
          seed = seed
        )
        results_eigen <- MCFunc(
          coef = lavaan::coef(object = fit),
          vcov = lavaan::vcov(object = fit),
          func = func,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "eigen",
          seed = seed
        )
        results_svd <- MCFunc(
          coef = lavaan::coef(object = fit),
          vcov = lavaan::vcov(object = fit),
          func = func,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "svd",
          seed = seed
        )
        set.seed(seed = seed)
        coefs <- MASS::mvrnorm(
          n = R,
          mu = lavaan::coef(object = fit),
          Sigma = lavaan::vcov(object = fit)
        )
        answers <- cbind(
          coefs,
          ab = coefs[, "a"] * coefs[, "b"]
        )
        ## Func
        coef <- as.data.frame(
          x = t(x = coefs)
        )
        results_func <- Func(
          coef = coef,
          func = func,
          est = lavaan::coef(object = fit),
          alpha = c(0.001, 0.01, 0.05)
        )
        testthat::expect_equal(
          object = results_func$thetahat$est,
          expected = lavaan::parameterEstimates(object = fit)[7, "est"],
          check.attributes = FALSE
        )
        testthat::expect_true(
          object = abs(
            x = .MCCI(
              object = results_func
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
  n = 1000L,
  R = 2000L,
  tol = 0.01,
  text = "test-semmcci-mc-func-simple-med"
)
