## ---- test-semmcci-zzz-heywood
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
        seed <- 42
        h0 <- "
          eta1 =~ 1.000 * y1
          eta1 =~ 0.991 * y2
          eta1 =~ 0.901 * y3
          eta1 =~ 1.399 * y4
          y1 ~~ 0.000 * y1
          y2 ~~ 1.000 * y2
          y3 ~~ 0.823 * y3
          y4 ~~ 0.710 * y4
        "
        sigmacap <- matrix(
          data = c(
            1.000, 0.991000, 0.901000, 1.399000,
            0.991, 1.982081, 0.892891, 1.386409,
            0.901, 0.892891, 1.634801, 1.260499,
            1.399, 1.386409, 1.260499, 2.667201
          ),
          nrow = 4
        )
        colnames(x = sigmacap) <- rownames(x = sigmacap) <- paste0("y", 1:4)
        model <- "
          eta1 =~ y1 + y2 + y3 + y4
        "
        fit <- lavaan::sem(
          model = model,
          sample.cov = sigmacap,
          sample.nobs = 50
        )
        print(x = lavaan::summary(object = fit))
        print(x = MCStd(mc = MC(lav = fit)))
        testthat::expect_true(
          object = TRUE
        )
      }
    )
  },
  text = "test-semmcci-zzz-heywood"
)
