## ---- test-external-semmcci-mc-simple-med-std
lapply(
  X = 1,
  FUN = function(i,
                 n,
                 R,
                 alpha,
                 tol,
                 text) {
    message(text)
    column_names <- c(
      "est",
      "se",
      "ll",
      "ul"
    )
    seed <- sample.int(
      n = .Machine$integer.max,
      size = 1
    )
    coefs <- stats::runif(
      n = 3,
      min = 0.0,
      max = 0.5
    )
    cp <- coefs[1]
    b <- coefs[2]
    a <- coefs[3]
    sigma2ey <- 1 - b^2 - cp^2 - 2 * a * b * cp
    sigma2em <- 1 - a^2
    sigma2x <- 1
    coefs <- c(
      cp = cp,
      b = b,
      a = a,
      ab = a * b
    )
    x <- stats::rnorm(n = n, sd = sqrt(x = sigma2x))
    m <- a * x + stats::rnorm(n = n, sd = sqrt(x = sigma2em))
    y <- cp * x + b * m + stats::rnorm(n = n, sd = sqrt(x = sigma2ey))
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
      fixed.x = FALSE
    )
    coefs <- lavaan::standardizedSolution(object = fit)$est.std
    unstd <- MC(
      lav = fit,
      R = R,
      alpha = alpha,
      seed = seed
    )
    results <- .MCCI(
      object = MCStd(
        mc = unstd,
        alpha = alpha
      )
    )
    results <- results[, c(1, 2, 4, 5)]
    colnames(x = results) <- column_names
    set.seed(seed = seed)
    answer <- MASS::mvrnorm(
      n = R,
      mu = lavaan::coef(object = fit),
      Sigma = lavaan::vcov(object = fit)
    )
    sigma2x_mc <- answer[, "sigma2x"]
    sigma2m_mc <- (
      answer[, "sigma2em"]
    ) + (
      answer[, "sigma2x"] * answer[, "a"]^2
    )
    sigma2y_mc <- (
      answer[, "sigma2ey"]
    ) + (
      answer[, "sigma2em"] * answer[, "b"]^2
    ) + (
      answer[, "cp"]^2 * answer[, "sigma2x"]
    ) + (
      answer[, "a"]^2 * answer[, "b"]^2 * answer[, "sigma2x"]
    ) + (
      2 * answer[, "a"] * answer[, "b"] * answer[, "cp"] * answer[, "sigma2x"]
    )
    cp_mc <- (
      sqrt(
        x = sigma2x_mc
      ) / sqrt(
        x = sigma2y_mc
      )
    ) * answer[, "cp"]
    b_mc <- (
      sqrt(
        x = sigma2m_mc
      ) / sqrt(
        x = sigma2y_mc
      )
    ) * answer[, "b"]
    a_mc <- (
      sqrt(
        x = sigma2x_mc
      ) / sqrt(
        x = sigma2m_mc
      )
    ) * answer[, "a"]
    ab_mc <- a_mc * b_mc
    answer <- cbind(
      cp = cp_mc,
      b = b_mc,
      a = a_mc,
      sigma2ey = answer[, "sigma2ey"],
      sigma2em = answer[, "sigma2em"],
      sigma2x = answer[, "sigma2x"],
      ab = ab_mc
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
  text = "test-external-semmcci-mc-simple-med-std"
)
