## ---- test-semmcci-mc-simple-med-std-defined-none
lapply(
  X = 1,
  FUN = function(i,
                 n,
                 text) {
    message(text)
    seed <- sample.int(
      n = .Machine$integer.max,
      size = 1
    )
    coefs <- stats::runif(
      n = 3,
      min = 0.0,
      max = 0.5
    )
    cp <- coefs[1]
    b <- coefs[2]
    a <- coefs[3]
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
    results_unstd <- MC(
      fit,
      R = 10L,
      alpha = c(0.001, 0.01, 0.05)
    )
    results_unstd$thetahatstar[3, ] <- lavaan::parameterEstimates(fit)$est
    results <- MCStd(results_unstd)
    testthat::test_that(
      text,
      {
        testthat::expect_equal(
          results$thetahat$est,
          lavaan::parameterEstimates(fit)$est,
          check.attributes = FALSE
        )
        testthat::expect_equal(
          results$thetahatstar.std[3, ],
          lavaan::standardizedSolution(fit)$est.std,
          check.attributes = FALSE
        )
        testthat::expect_equal(
          results$ci.std["cp", "0.05%"],
          quantile(results$thetahatstar.std[, "cp"], .0005),
          check.attributes = FALSE
        )
      }
    )
  },
  n = 100L,
  text = "test-semmcci-mc-simple-med-std-defined-none"
)
