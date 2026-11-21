## ---- test-semmcci-smoke
lapply(
  X = 1,
  FUN = function(i,
                 n,
                 R,
                 text) {
    message(text)

    testthat::test_that(
      desc = paste(text, "MC runs on CRAN"),
      code = {
        set.seed(seed = 42)
        x <- stats::rnorm(n = n)
        m <- 0.50 * x + stats::rnorm(n = n)
        y <- 0.25 * x + 0.50 * m + stats::rnorm(n = n)
        data <- data.frame(x, m, y)
        model <- "
          y ~ cp * x + b * m
          m ~ a * x
          ab := a * b
        "
        fit <- lavaan::sem(
          data = data,
          model = model
        )
        out <- MC(
          lav = fit,
          R = R,
          alpha = 0.05
        )
        testthat::expect_true(
          object = inherits(
            x = out,
            what = "semmcci"
          )
        )
        testthat::expect_equal(
          object = nrow(x = out$thetahatstar),
          expected = R
        )
      }
    )
  },
  n = 100L,
  R = 100L,
  text = "test-semmcci-smoke"
)
