## ---- test-semmcci-mvn
lapply(
  X = seq_len(3),
  FUN = function(k,
                 n,
                 tol,
                 text) {
    message(text)
    seed <- 42
    set.seed(42)
    mu <- stats::runif(
      n = k,
      min = -1,
      max = 1
    )
    Sigma <- toeplitz((k:1) / k)
    norm <- matrix(
      data = stats::rnorm(n = n * k),
      nrow = n,
      ncol = k
    )
    y1 <- .MVNChol(
      norm = norm,
      mat = chol(Sigma)
    ) + rep(
      x = mu,
      times = rep(
        x = n,
        times = k
      )
    )
    y2 <- .MVNEigen(
      norm = norm,
      mat = eigen(Sigma, symmetric = TRUE)
    ) + rep(
      x = mu,
      times = rep(
        x = n,
        times = k
      )
    )
    testthat::test_that(
      paste(text, "means", "y1"),
      {
        testthat::expect_true(
          all(
            abs(
              colMeans(y1) - mu
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "means", "y2"),
      {
        testthat::expect_true(
          all(
            abs(
              colMeans(y2) - mu
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "covariances", "y1"),
      {
        testthat::expect_true(
          all(
            abs(
              stats::cov(y1) - Sigma
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "covariances", "y2"),
      {
        testthat::expect_true(
          all(
            abs(
              stats::cov(y2) - Sigma
            ) <= tol
          )
        )
      }
    )
  },
  n = 10000L,
  tol = 0.05,
  text = "test-semmcci-mvn"
)
