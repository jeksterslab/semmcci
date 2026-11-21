## ---- test-semmcci-mc-moment
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)

    set.seed(seed = 42)

    testthat::skip_on_cran()

    testthat::test_that(
      desc = paste(text, "MCCI error"),
      code = {
        testthat::skip_on_cran()
        lower <- "
          11.834
          6.947   9.364
          6.819   5.091  12.532
          4.783   5.028   7.495   9.986
          -3.839  -3.889  -3.841  -3.625  9.610
          -21.899 -18.831 -21.748 -18.775 35.522 450.288
        "
        sample_cov <- lavaan::lav_getcov(
          x = lower,
          names = c(
            "anomia67",
            "powerless67",
            "anomia71",
            "powerless71",
            "education",
            "sei"
          )
        )
        # classic wheaton et al. model
        model <- "
          # latent variables
          ses     =~ education + sei
          alien67 =~ anomia67 + powerless67
          alien71 =~ anomia71 + powerless71
          # regressions
          alien71 ~ alien67 + ses
          alien67 ~ ses
          # correlated residuals
          anomia67 ~~ anomia71
          powerless67 ~~ powerless71
        "
        fit <- lavaan::sem(
          model = model,
          sample.cov = sample_cov,
          sample.nobs = 932
        )
        testthat::expect_error(
          object = MCMI(lav = fit)
        )
        MC(lav = fit)
      }
    )
  },
  text = "test-semmcci-mc-moment"
)
