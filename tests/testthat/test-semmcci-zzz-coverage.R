## ---- test-semmcci-zzz-coverage
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)

    set.seed(42)

    if (!identical(Sys.getenv("NOT_CRAN"), "true") && !interactive()) {
      message("CRAN: tests skipped.")
      # nolint start
      return(invisible(NULL))
      # nolint end
    }

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
