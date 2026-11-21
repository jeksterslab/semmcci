## ---- test-semmcci-psd
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)

    set.seed(seed = 42)

    testthat::skip_on_cran()

    testthat::test_that(
      desc = paste(text, "singular"),
      code = {
        testthat::skip_on_cran()
        location <- c(
          y1 = 0,
          y2 = 0,
          y3 = 0
        )
        scale <- matrix(
          data = c(1, 1, 0, 1, 1, 0, 0, 0, 1),
          ncol = 3
        )
        for (decomposition in c("chol", "eigen", "svd")) {
          out <- semmcci:::.ThetaHatStar(
            R = 100L,
            scale = scale,
            location = location,
            decomposition = decomposition,
            pd = TRUE
          )
          testthat::expect_equal(
            object = dim(
              x = out$thetahatstar
            ),
            expected = c(100L, 3L)
          )
          testthat::expect_equal(
            object = out$thetahatstar[, 1],
            expected = out$thetahatstar[, 2],
            tolerance = 1e-5
          )
        }
      }
    )
  },
  text = "test-semmcci-psd"
)
