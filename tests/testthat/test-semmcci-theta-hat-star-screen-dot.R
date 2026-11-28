## ---- test-semmcci-theta-hat-star-screen-dot
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)
    testthat::skip_on_cran()
    location <- c(
      a = 0.5,
      b = 1
    )
    scale <- matrix(
      data = c(1, 0.3, 0.3, 1),
      nrow = 2
    )
    r <- 200L
    testthat::test_that(
      desc = paste(text, "no rejection: counts and the same draws"),
      code = {
        set.seed(seed = 42)
        expected <- semmcci:::.ThetaHatStar(
          R = r,
          scale = scale,
          location = location
        )$thetahatstar
        set.seed(seed = 42)
        out <- semmcci:::.ThetaHatStarScreen(
          R = r,
          scale = scale,
          location = location,
          screen = function(x) {
            rep(x = TRUE, times = nrow(x = x))
          }
        )
        testthat::expect_equal(
          object = attr(x = out, which = "screen"),
          expected = c(
            requested = r,
            retained = r,
            rejected = 0L,
            redraws = 0L,
            exhausted = 0L,
            iter = 1000L
          )
        )
        attr(x = out, which = "screen") <- NULL
        testthat::expect_identical(
          object = out,
          expected = expected
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "some rejected and redrawn"),
      code = {
        set.seed(seed = 42)
        out <- semmcci:::.ThetaHatStarScreen(
          R = r,
          scale = scale,
          location = location,
          screen = function(x) {
            x[, 1] > 0
          }
        )
        counts <- attr(x = out, which = "screen")
        testthat::expect_true(
          object = counts[["rejected"]] > 0
        )
        testthat::expect_true(
          object = counts[["redraws"]] >= counts[["rejected"]]
        )
        testthat::expect_equal(
          object = counts[["exhausted"]],
          expected = 0L
        )
        testthat::expect_equal(
          object = counts[["retained"]],
          expected = nrow(x = out)
        )
        testthat::expect_equal(
          object = nrow(x = out),
          expected = r
        )
        testthat::expect_true(
          object = all(out[, 1] > 0)
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "every draw exhausted"),
      code = {
        set.seed(seed = 42)
        out <- semmcci:::.ThetaHatStarScreen(
          R = 10L,
          scale = scale,
          location = location,
          screen = function(x) {
            rep(x = FALSE, times = nrow(x = x))
          },
          iter = 3L
        )
        testthat::expect_equal(
          object = attr(x = out, which = "screen"),
          expected = c(
            requested = 10L,
            retained = 0L,
            rejected = 10L,
            redraws = 30L,
            exhausted = 10L,
            iter = 3L
          )
        )
        testthat::expect_equal(
          object = nrow(x = out),
          expected = 0L
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "appended values"),
      code = {
        set.seed(seed = 42)
        out <- semmcci:::.ThetaHatStarScreen(
          R = r,
          scale = scale,
          location = location,
          screen = function(x) {
            x[, 1] > 0 & x[, 3] == 9
          },
          append = 9
        )
        testthat::expect_equal(
          object = ncol(x = out),
          expected = 3L
        )
        testthat::expect_true(
          object = all(out[, 3] == 9)
        )
        testthat::expect_equal(
          object = attr(x = out, which = "screen")[["exhausted"]],
          expected = 0L
        )
      }
    )
  },
  text = "test-semmcci-theta-hat-star-screen-dot"
)
