## ---- test-semmcci-mc-latent-med-std-defined-none
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
      textual ~ visual
      speed ~ textual
    "
    fit <- lavaan::sem(
      model = model,
      data = data
    )
    set.seed(seed)
    results_unstd <- MC(
      fit,
      R = 10L,
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
          results$thetahatstar.std[3, ],
          lavaan::standardizedSolution(fit)$est.std,
          check.attributes = FALSE
        )
        testthat::expect_equal(
          results$ci.std["textual~visual", "0.05%"],
          quantile(results$thetahatstar.std[, "textual~visual"], .0005),
          check.attributes = FALSE
        )
      }
    )
  },
  n = 1000L,
  text = "test-semmcci-mc-latent-med-std-defined-none"
)