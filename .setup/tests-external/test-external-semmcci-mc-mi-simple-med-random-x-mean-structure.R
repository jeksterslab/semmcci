## ---- test-external-semmcci-mc-mi-simple-med-random-x-mean-structure
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
      sigmasqx = 1,
      sigmasqm = 1,
      sigmasqy = 1,
      mux = 0,
      mum = 0,
      muy = 0,
      ab = a * b
    )
    x <- rnorm(n = n)
    m <- a * x + rnorm(n = n)
    y <- cp * x + b * m + rnorm(n = n)
    data <- data.frame(x, m, y)
    model <- "
      y ~ cp * x + b * m
      m ~ a * x
      y ~~ sigma2ey * y
      m ~~ sigma2em * m
      x ~~ sigma2x * x
      ab := a * b
    "
    fit <- lavaan::sem(
      data = data,
      model = model,
      fixed.x = FALSE,
      meanstructure = TRUE
    )
    results <- .MCCI(
      MCMI(
        fit,
        R = R,
        alpha = alpha,
        seed = seed,
        mi = mice::mice(
          data,
          m = 5,
          seed = seed,
          print = FALSE
        )
      )
    )
    results <- results[, c(1, 2, 4, 5)]
    colnames(results) <- column_names
    set.seed(seed)
    answer <- MASS::mvrnorm(
      n = R,
      mu = lavaan::coef(fit),
      Sigma = lavaan::vcov(fit)
    )
    answer <- cbind(
      answer,
      ab = answer[, "a"] * answer[, "b"]
    )
    expected <- colMeans(answer)
    se <- sqrt(diag(stats::var(answer)))
    prob_ll <- alpha / 2
    prob_ul <- 1 - prob_ll
    answer <- lapply(
      X = as.data.frame(answer),
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
    colnames(answer) <- column_names
    testthat::test_that(
      paste(text, "coefs"),
      {
        testthat::expect_true(
          all(abs(coefs - as.vector(results[, "est"])) <= tol)
        )
        testthat::expect_true(
          all(abs(coefs - expected) <= tol)
        )
      }
    )
  },
  n = 10000L,
  R = 10000L,
  alpha = 0.05,
  tol = 0.05,
  text = "test-external-semmcci-mc-mi-simple-med-random-x-mean-structure"
)
