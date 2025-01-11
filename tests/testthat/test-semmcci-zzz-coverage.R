## ---- test-semmcci-zzz-coverage
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)
    testthat::test_that(
      paste(text, "coverage"),
      {
        testthat::skip_on_cran()
        semmcci:::.MICombine(
          coefs = list(rep(x = 0, times = 3), rep(x = 0, times = 3)),
          vcovs = list(diag(3), diag(3)),
          M = 2,
          k = 3,
          adj = FALSE
        )
        testthat::expect_true(
          TRUE
        )
      }
    )
  },
  text = "test-semmcci-zzz-coverage"
)
