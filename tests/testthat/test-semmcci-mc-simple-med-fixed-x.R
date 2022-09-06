## ---- test-semmcci-mc-simple-med-fixed-x
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
    coefs <- c(coefs, 1, 1, 1, a * b)
    x <- rnorm(n = n)
    m <- a * x + rnorm(n = n)
    y <- cp * x + b * m + rnorm(n = n)
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
    set.seed(seed)
    result <- mc(
      fit,
      R = R,
      alpha = alpha
    )$ci
    result <- result[, c(1, 2, 4, 5)]
    colnames(result) <- column_names
    set.seed(seed)
    answer <- MASS::mvrnorm(
      n = R,
      mu = lavaan::coef(fit),
      Sigma = lavaan::vcov(fit)
    )
    answer <- cbind(
      answer,
      `x~~x` = result["x~~x", "est"],
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
      est = result[, "est"],
      se = se,
      answer
    )
    colnames(answer) <- column_names
    answer["x~~x", "se"] <- answer["x~~x", "ll"] <- answer["x~~x", "ul"] <- NA
    testthat::test_that(
      paste(text, "coefs"),
      {
        testthat::expect_true(
          all(abs(coefs - as.vector(result[, "est"])) <= tol)
        )
        testthat::expect_true(
          all(abs(coefs - expected) <= tol)
        )
      }
    )
    testthat::test_that(
      paste(text),
      {
        testthat::expect_true(
          all.equal(answer, result)
        )
      }
    )
  },
  n = 100000,
  R = 20000L,
  alpha = 0.05,
  tol = 0.05,
  text = "test-semmcci-mc-simple-med-fixed-x"
)
