## ---- test-semmcci-mc-latent-med-defined-inequality
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
    data <- lavaan::HolzingerSwineford1939
    model <- "
      visual  =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
      speed   =~ x7 + x8 + x9
      textual ~ a * visual
      speed ~ b * textual
      ab :=  a * b
      a > 0
    "
    fit <- lavaan::sem(
      model = model,
      data = data
    )
    set.seed(seed)
    results <- MC(
      fit,
      R = 10L,
      alpha = c(0.001, 0.01, 0.05)
    )
    set.seed(seed)
    answers <- MASS::mvrnorm(
      n = 10L,
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
          results$thetahatstar[, colnames(answers)],
          answers
        )
        testthat::expect_equal(
          results$thetahat$est[names(lavaan::coef(fit))],
          as.vector(lavaan::coef(fit)),
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
  text = "test-semmcci-mc-latent-med-defined-inequality"
)
