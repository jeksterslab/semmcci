## ---- test-semmcci-mc-simple-med-defined-inequality
lapply(
  X = 1,
  FUN = function(i,
                 n,
                 text) {
    R <- 10L
    message(text)
    seed <- sample.int(
      n = .Machine$integer.max,
      size = 1
    )
    set.seed(42) # seed for data generation
    cp <- 0
    b <- 0.50
    a <- 0.50
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
    set.seed(seed)
    results <- MC(
      fit,
      R = R,
      alpha = c(0.001, 0.01, 0.05)
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
    testthat::test_that(
      text,
      {
        testthat::expect_equal(
          results$thetahatstar[, 1:7],
          answers
        )
        testthat::expect_equal(
          results$thetahat$est[1:7],
          lavaan::parameterEstimates(fit)$est,
          check.attributes = FALSE
        )
        testthat::expect_equal(
          results$ci["ab", "0.05%"],
          quantile(answers[, "ab"], .0005),
          check.attributes = FALSE
        )
      }
    )
  },
  n = 100L,
  text = "test-semmcci-mc-simple-med-defined-inequality"
)
