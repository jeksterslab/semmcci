## ---- test-external-semmcci-mc-mi-simple-med-fixed-x
lapply(
  X = 1,
  FUN = function(i, n, R, alpha, tol, text) {
    message(text)
    column_names <- c(
      "est",
      "se",
      "ll",
      "ul"
    )
    seed <- sample.int(n = .Machine$integer.max, size = 1)
    coefs <- stats::runif(n = 3, min = 0.0, max = 0.5)
    cp <- coefs[1]
    b <- coefs[2]
    a <- coefs[3]
    coefs <- c(
      coefs,
      sigmasqm = 1,
      sigmasqy = 1,
      ab = a * b
    )
    x <- stats::rnorm(n = n)
    m <- a * x + stats::rnorm(n = n)
    y <- cp * x + b * m + stats::rnorm(n = n)
    data <- data.frame(x, m, y)
    model <- "
      y ~ cp * x + b * m
      m ~ a * x
      y ~~ sigma2ey * y
      m ~~ sigma2em * m
      ab := a * b
    "
    fit <- lavaan::sem(
      data = data,
      model = model,
      fixed.x = TRUE
    )
    results <- .MCCI(
      object = MCMI(
        lav = fit,
        R = R,
        alpha = alpha,
        seed = seed,
        mi = mice::mice(
          data = data,
          m = 5,
          seed = seed,
          print = FALSE
        )
      )
    )
    results <- results[, c(1, 2, 4, 5)]
    colnames(x = results) <- column_names
    results <- results[
      c(
        "cp",
        "b",
        "a",
        "sigma2ey",
        "sigma2em",
        "ab"
      ),
    ]
    set.seed(seed = seed)
    answer <- MASS::mvrnorm(
      n = R,
      mu = lavaan::coef(object = fit),
      Sigma = lavaan::vcov(object = fit)
    )
    answer <- cbind(
      answer,
      ab = answer[, "a"] * answer[, "b"]
    )
    expected <- colMeans(x = answer)
    se <- sqrt(x = diag(x = stats::var(x = answer)))
    prob_ll <- alpha / 2
    prob_ul <- 1 - prob_ll
    answer <- lapply(
      X = as.data.frame(x = answer),
      FUN = stats::quantile,
      probs = c(prob_ll, prob_ul)
    )
    answer <- do.call(
      what = "rbind",
      args = answer
    )
    answer <- cbind(
      est = results[, "est"],
      se = se,
      answer
    )
    colnames(x = answer) <- column_names
    testthat::test_that(
      desc = paste(text, "coefs"),
      code = {
        testthat::expect_true(
          object = all(abs(x = coefs - as.vector(x = results[, "est"])) <= tol)
        )
        testthat::expect_true(
          object = all(abs(x = coefs - expected) <= tol)
        )
      }
    )
  },
  n = 10000L,
  R = 10000L,
  alpha = 0.05,
  tol = 0.05,
  text = "test-external-semmcci-mc-mi-simple-med-fixed-x"
)
