## ---- test-semmcci-npd
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)

    set.seed(seed = 42)

    testthat::skip_on_cran()

    testthat::test_that(
      desc = paste(text, "chol"),
      code = {
        testthat::skip_on_cran()
        location <- c(
          y1 = 0,
          y2 = 0,
          y3 = 0
        )
        scale <- matrix(
          data = c(1, 2, 3, 2, 4, 5, 3, 5, 6),
          ncol = 3
        )
        testthat::expect_error(
          object = semmcci:::.ThetaHatStar(
            R = 2000L,
            scale = scale,
            location = location,
            decomposition = "chol",
            pd = TRUE
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "eigen"),
      code = {
        testthat::skip_on_cran()
        location <- c(
          y1 = 0,
          y2 = 0,
          y3 = 0
        )
        scale <- matrix(
          data = c(1, 2, 3, 2, 4, 5, 3, 5, 6),
          ncol = 3
        )
        testthat::expect_error(
          object = semmcci:::.ThetaHatStar(
            R = 2000L,
            scale = scale,
            location = location,
            decomposition = "eigen",
            pd = TRUE
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "svd"),
      code = {
        testthat::skip_on_cran()
        location <- c(
          y1 = 0,
          y2 = 0,
          y3 = 0
        )
        scale <- matrix(
          data = c(1, 2, 3, 2, 4, 5, 3, 5, 6),
          ncol = 3
        )
        testthat::expect_error(
          object = semmcci:::.ThetaHatStar(
            R = 2000L,
            scale = scale,
            location = location,
            decomposition = "svd",
            pd = TRUE
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "wrong"),
      code = {
        testthat::skip_on_cran()
        location <- c(
          y1 = 0,
          y2 = 0,
          y3 = 0
        )
        scale <- matrix(
          data = c(1, 2, 3, 2, 4, 5, 3, 5, 6),
          ncol = 3
        )
        testthat::expect_error(
          object = semmcci:::.ThetaHatStar(
            R = 2000L,
            scale = scale,
            location = location,
            decomposition = "wrong",
            pd = FALSE
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "coverage"),
      code = {
        testthat::skip_on_cran()
        location <- c(
          y1 = 0,
          y2 = 0,
          y3 = 0
        )
        scale <- matrix(
          data = c(1, 2, 3, 2, 4, 5, 3, 5, 6),
          ncol = 3
        )
        semmcci:::.ThetaHatStar(
          R = 2000L,
          scale = scale,
          location = location,
          decomposition = "eigen",
          pd = FALSE
        )
        testthat::expect_true(
          object = TRUE
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "chol falls back to eigen for a singular matrix"),
      code = {
        testthat::skip_on_cran()
        v <- c(1, 2, 3)
        scale <- tcrossprod(x = v)
        out <- semmcci:::.ThetaHatStar(
          R = 100L,
          scale = scale,
          location = c(y1 = 0, y2 = 0, y3 = 0),
          decomposition = "chol",
          pd = FALSE
        )
        testthat::expect_identical(
          object = out$decomposition,
          expected = "eigen"
        )
        testthat::expect_identical(
          object = dim(x = out$thetahatstar),
          expected = c(100L, 3L)
        )
      }
    )
  },
  text = "test-semmcci-npd"
)
