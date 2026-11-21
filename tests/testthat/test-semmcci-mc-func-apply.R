## ---- test-semmcci-mc-func-apply
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)

    testthat::skip_on_cran()

    x <- data.frame(
      a = c(1, 2, 3),
      b = c(4, 5, 6),
      c = c(7, 8, 9),
      d = c(10, 11, 12)
    )
    func <- function(x,
                     k) {
      sum(x) * k
    }
    expected <- lapply(
      X = x,
      FUN = func,
      k = 2
    )

    testthat::test_that(
      desc = paste(text, "serial"),
      code = {
        testthat::skip_on_cran()
        for (ncores in list(NULL, 1L)) {
          testthat::expect_identical(
            object = semmcci:::.MCFuncApply(
              X = x,
              FUN = func,
              k = 2,
              ncores = ncores
            ),
            expected = expected
          )
        }
      }
    )

    testthat::test_that(
      desc = paste(text, "parallel"),
      code = {
        testthat::skip_on_cran()
        testthat::skip_on_os(os = "windows")
        testthat::expect_identical(
          object = semmcci:::.MCFuncApply(
            X = x,
            FUN = func,
            k = 2,
            ncores = 2L
          ),
          expected = expected
        )
      }
    )
  },
  text = "test-semmcci-mc-func-apply"
)
