## ---- test-semmcci-mc-simple-med-defined-mi-mice
lapply(
  X = 1,
  FUN = function(i,
                 n,
                 R,
                 m,
                 tol,
                 text) {
    message(text)

    set.seed(seed = 42)

    testthat::skip_on_cran()

    testthat::test_that(
      desc = paste(text, "mice"),
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
        x <- stats::rnorm(n = n, sd = sqrt(x = sigma2x))
        m <- a * x + stats::rnorm(n = n, sd = sqrt(x = sigma2em))
        y <- cp * x + b * m + stats::rnorm(n = n, sd = sqrt(x = sigma2ey))
        data <- data.frame(x, m, y)
        data[1, "x"] <- NA
        data[2, "m"] <- NA
        data[3, "y"] <- NA
        model <- "
          y ~ cp * x + b * m
          m ~ a * x
          ab := a * b
        "
        fit <- lavaan::sem(
          data = data,
          model = model,
          fixed.x = FALSE,
          missing = "fiml"
        )
        results_mice <- MCMI(
          lav = fit,
          R = R,
          alpha = c(0.001, 0.01, 0.05),
          decomposition = "eigen",
          seed = seed,
          mi = mice::mice(
            data = data,
            print = FALSE,
            m = 5L,
            seed = seed
          )
        )
        set.seed(seed = seed)
        answers <- MASS::mvrnorm(
          n = R,
          mu = lavaan::coef(object = fit),
          Sigma = lavaan::vcov(object = fit)
        )
        answers <- cbind(
          answers,
          ab = answers[, "a"] * answers[, "b"]
        )
        testthat::expect_equal(
          object = results_mice$thetahat$est,
          expected = lavaan::parameterEstimates(object = fit)$est,
          check.attributes = FALSE,
          tolerance = tol
        )
        testthat::expect_true(
          object = abs(
            x = .MCCI(
              object = results_mice
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
  m = 5,
  tol = 0.05,
  text = "test-semmcci-mc-simple-med-defined-mi-mice"
)
