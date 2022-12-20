## ---- test-semmcci-npd
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)
    location <- c(
      y1 = 0,
      y2 = 0,
      y3 = 0
    )
    scale <- matrix(
      data = c(1, 2, 3, 2, 4, 5, 3, 5, 6),
      ncol = 3
    )
    testthat::test_that(
      paste(text, "chol"),
      {
        testthat::expect_error(
          semmcci:::.ThetaHatStar(
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
      paste(text, "eigen"),
      {
        testthat::expect_error(
          semmcci:::.ThetaHatStar(
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
      paste(text, "svd"),
      {
        testthat::expect_error(
          semmcci:::.ThetaHatStar(
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
      paste(text, "wrong"),
      {
        testthat::expect_error(
          semmcci:::.ThetaHatStar(
            R = 2000L,
            scale = scale,
            location = location,
            decomposition = "wrong",
            pd = FALSE
          )
        )
      }
    )
    # coverage
    semmcci:::.ThetaHatStar(
      R = 2000L,
      scale = scale,
      location = location,
      decomposition = "eigen",
      pd = FALSE
    )
  },
  text = "test-semmcci-npd"
)
