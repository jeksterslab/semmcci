## ---- test-semmcci-mc-simple-med-std
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
      x ~~ sigma2x * x
      ab := a * b
    "
    fit <- lavaan::sem(
      data = data,
      model = model,
      fixed.x = FALSE
    )
    set.seed(seed)
    unstd <- mc(
      fit,
      R = R,
      alpha = alpha
    )
    result <- mc_std(
      unstd
    )$ci.std
    result <- result[, c(1, 2, 4, 5)]
    colnames(result) <- column_names
    print(result)
    set.seed(seed)
    answer <- MASS::mvrnorm(
      n = R,
      mu = lavaan::coef(fit),
      Sigma = lavaan::vcov(fit)
    )
    sigma2x_mc <- answer[, "sigma2x"]
    sigma2m_mc <- answer[, "sigma2em"] + answer[, "sigma2x"] * answer[, "a"]^2
    sigma2y_mc <- answer[, "sigma2ey"] + answer[, "sigma2em"] * answer[, "b"]^2 + answer[, "sigma2x"] * (answer[, "cp"] + answer[, "b"] * answer[, "a"])^2
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
    ab_mc <- answer[, "a"] * answer[, "b"] * (
      sqrt(
        sigma2x_mc
      ) / sqrt(
        sigma2y_mc
      )
    )
    # ab_mc <- a_mc * b_mc
    # sigma2x_mc <- answer[,  "sigma2x"] / answer[,  "sigma2x"]
    # sigma2em_mc <- 1 - a_mc^2
    # sigma2ey_mc <- 1 - (
    #  sigma2em_mc * b_mc^2 + sigma2x_mc * (cp_mc + b_mc * a_mc)^2
    # )
    answer <- cbind(
      answer,
      ab = ab_mc
    )
    answer[, "cp"] <- cp_mc
    answer[, "b"] <- b_mc
    answer[, "a"] <- a_mc
    # answer[, "sigma2x"] <- sigma2x_mc
    # answer[, "sigma2em"] <- sigma2em_mc
    # answer[, "sigma2ey"] <- sigma2ey_mc
    expected <- colMeans(answer)
    print(coefs)
    print(expected)
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
    print(answer)
    #    testthat::test_that(
    #      paste(text, "coefs"),
    #      {
    #        testthat::expect_true(
    #          all(abs(coefs - as.vector(result[, "est"])) <= 0.01)
    #        )
    #        testthat::expect_true(
    #          all(abs(coefs - expected) <= 0.01)
    #        )
    #      }
    #    )
    #    testthat::test_that(
    #      paste(text),
    #      {
    #        testthat::expect_true(
    #          all.equal(answer, result)
    #        )
    #      }
    #    )
  },
  n = 1000,
  R = 100,
  alpha = 0.05,
  tol = 0.01,
  text = "test-semmcci-mc-simple-med-std"
)
