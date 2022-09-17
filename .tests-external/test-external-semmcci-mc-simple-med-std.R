## ---- test-semmcci-mc-simple-med-std
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
    x <- rnorm(n = n, sd = sqrt(sigma2x))
    m <- a * x + rnorm(n = n, sd = sqrt(sigma2em))
    y <- cp * x + b * m + rnorm(n = n, sd = sqrt(sigma2ey))
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
    coefs <- lavaan::standardizedSolution(fit)$est.std
    set.seed(seed)
    unstd <- MC(
      fit,
      R = R,
      alpha = alpha
    )
    result <- MCStd(
      unstd,
      alpha = alpha
    )$ci_std
    result <- result[, c(1, 2, 4, 5)]
    colnames(result) <- column_names
    set.seed(seed)
    answer <- MASS::mvrnorm(
      n = R,
      mu = lavaan::coef(fit),
      Sigma = lavaan::vcov(fit)
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
        sigma2x_mc
      ) / sqrt(
        sigma2y_mc
      )
    ) * answer[, "cp"]
    b_mc <- (
      sqrt(
        sigma2m_mc
      ) / sqrt(
        sigma2y_mc
      )
    ) * answer[, "b"]
    a_mc <- (
      sqrt(
        sigma2x_mc
      ) / sqrt(
        sigma2m_mc
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
  },
  n = 10000L,
  R = 10000L,
  alpha = 0.05,
  tol = 0.05,
  text = "test-semmcci-mc-simple-med-std"
)

