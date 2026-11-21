## ---- test-semmcci-zzz-coverage
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)

    set.seed(seed = 42)

    testthat::skip_on_cran()

    testthat::test_that(
      desc = paste(text, "coverage"),
      code = {
        testthat::skip_on_cran()
        semmcci:::.MICombine(
          coefs = list(rep(x = 0, times = 3), rep(x = 0, times = 3)),
          vcovs = list(diag(x = 3), diag(x = 3)),
          M = 2,
          k = 3,
          adj = FALSE
        )
        testthat::expect_true(
          object = TRUE
        )
      }
    )
  },
  text = "test-semmcci-zzz-coverage"
)
