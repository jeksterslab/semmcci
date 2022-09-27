## ---- test-semmcci-mc-latent-med-defined-none
lapply(
  X = 1,
  FUN = function(i,
                 R,
                 tol,
                 text) {
    message(text)
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
    testthat::test_that(
      text,
      {
        testthat::expect_equal(
          results$thetahat$est[names(lavaan::coef(fit))],
          as.vector(lavaan::coef(fit)),
          check.attributes = FALSE
        )
        testthat::expect_true(
          abs(
            .MCCI(results)["visual~~textual", "0.05%"] - quantile(answers[, "visual~~textual"], .0005)
          ) <= tol
        )
      }
    )
  },
  R = 1000L,
  tol = 0.05,
  text = "test-semmcci-mc-latent-med-defined-none"
)
