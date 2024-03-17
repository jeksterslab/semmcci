## ---- test-semmcci-mc-func-simple-med
lapply(
  X = 1,
  FUN = function(i,
                 n,
                 R,
                 tol,
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
      ab := a * b
    "
    fit <- lavaan::sem(
      data = data,
      model = model,
      fixed.x = FALSE
    )
    func <- function(x) {
      ab <- x[2] * x[3]
      names(ab) <- "ab"
      return(
        ab
      )
    }
    ## MCFunc
    run <- TRUE
    tryCatch(
      {
        results_chol <- MCFunc(
          coef = lavaan::coef(fit),
          vcov = lavaan::vcov(fit),
          func = func,
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
    results_eigen <- MCFunc(
      coef = lavaan::coef(fit),
      vcov = lavaan::vcov(fit),
      func = func,
      R = R,
      alpha = c(0.001, 0.01, 0.05),
      decomposition = "eigen",
      seed = seed
    )
    results_svd <- MCFunc(
      coef = lavaan::coef(fit),
      vcov = lavaan::vcov(fit),
      func = func,
      R = R,
      alpha = c(0.001, 0.01, 0.05),
      decomposition = "svd",
      seed = seed
    )
    set.seed(seed)
    coefs <- MASS::mvrnorm(
      n = R,
      mu = lavaan::coef(fit),
      Sigma = lavaan::vcov(fit)
    )
    answers <- cbind(
      coefs,
      ab = coefs[, "a"] * coefs[, "b"]
    )
    ## Func
    coef <- as.data.frame(
      t(coefs)
    )
    results_func <- Func(
      coef = coef,
      func = func,
      est = lavaan::coef(fit),
      alpha = c(0.001, 0.01, 0.05)
    )
    if (run) {
      testthat::test_that(
        paste(text, "chol"),
        {
          testthat::expect_equal(
            results_chol$thetahat$est,
            lavaan::parameterEstimates(fit)[7, "est"],
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
    }
    testthat::test_that(
      paste(text, "eigen"),
      {
        testthat::expect_equal(
          results_eigen$thetahat$est,
          lavaan::parameterEstimates(fit)[7, "est"],
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
        testthat::expect_equal(
          results_svd$thetahat$est,
          lavaan::parameterEstimates(fit)[7, "est"],
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
    testthat::test_that(
      paste(text, "func"),
      {
        testthat::expect_equal(
          results_func$thetahat$est,
          lavaan::parameterEstimates(fit)[7, "est"],
          check.attributes = FALSE
        )
        testthat::expect_true(
          abs(
            .MCCI(
              results_func
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
  tol = 0.01,
  text = "test-semmcci-mc-func-simple-med"
)
