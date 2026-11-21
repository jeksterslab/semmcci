## ---- test-semmcci-mc-methods
lapply(
  X = 1,
  FUN = function(i,
                 R,
                 text) {
    message(text)

    set.seed(seed = 42)

    testthat::skip_on_cran()

    testthat::test_that(
      desc = paste(text, "methods"),
      code = {
        testthat::skip_on_cran()
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
        set.seed(seed = seed)
        unstd <- MC(
          lav = fit,
          R = R,
          alpha = c(0.001, 0.01, 0.05)
        )
        miunstd <- MCMI(
          lav = fit,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          mi = mice::mice(
            data = data,
            print = 0
          )
        )
        std <- MCStd(mc = unstd)
        mistd <- MCStd(mc = miunstd)
        print.semmcci(x = unstd)
        print.semmcci(x = miunstd)
        print.semmcci(x = std)
        print.semmcci(x = mistd)
        summary.semmcci(object = unstd)
        summary.semmcci(object = miunstd)
        summary.semmcci(object = std)
        summary.semmcci(object = mistd)
        coef.semmcci(object = unstd)
        coef.semmcci(object = miunstd)
        coef.semmcci(object = std)
        coef.semmcci(object = mistd)
        vcov.semmcci(object = unstd)
        vcov.semmcci(object = miunstd)
        vcov.semmcci(object = std)
        vcov.semmcci(object = mistd)
        confint.semmcci(object = unstd)
        confint.semmcci(object = miunstd)
        confint.semmcci(object = std)
        confint.semmcci(object = mistd)
        testthat::expect_true(
          object = TRUE
        )
      }
    )
  },
  R = 100L,
  text = "test-semmcci-mc-methods"
)
