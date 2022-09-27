## ---- test-semmcci-mc-latent-med-std-defined
lapply(
  X = 1,
  FUN = function(i,
                 R,
                 tol,
                 text) {
    message(text)
    seed <- 42
    data <- lavaan::HolzingerSwineford1939
    model <- "
      visual  =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
      speed   =~ x7 + x8 + x9
      textual ~ a * visual
      speed ~ b * textual
      ab := a * b
    "
    fit <- lavaan::sem(
      model = model,
      data = data
    )
    set.seed(seed)
    results_unstd <- MC(
      fit,
      R = R,
      alpha = c(0.001, 0.01, 0.05)
    )
    # insert original estimate on the third row
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
          results$thetahatstar_std[3, ],
          lavaan::standardizedSolution(fit)$est.std,
          check.attributes = FALSE
        )
        testthat::expect_equal(
          .MCCI(results)["ab", "0.05%"],
          quantile(results$thetahatstar_std[, "ab"], .0005),
          check.attributes = FALSE
        )
      }
    )
  },
  R = 1000L,
  tol = 0.05,
  text = "test-semmcci-mc-latent-med-std-defined"
)
