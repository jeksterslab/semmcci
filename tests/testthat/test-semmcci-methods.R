## ---- test-semmcci-mc-methods
lapply(
  X = 1,
  FUN = function(i,
                 R,
                 text) {
    message(text)
    seed <- 42
    data <- lavaan::HolzingerSwineford1939
    # cfa
    model <- "
      visual  =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
      speed   =~ x7 + x8 + x9
    "
    fit <- lavaan::cfa(
      model = model,
      data = data
    )
    set.seed(seed)
    unstd <- MC(
      fit,
      R = R,
      alpha = c(0.001, 0.01, 0.05)
    )
    miunstd <- MCMI(
      fit,
      R = R,
      alpha = c(0.001, 0.01, 0.05),
      mi = mice::mice(
        data = data,
        print = 0
      )
    )
    std <- MCStd(unstd)
    mistd <- MCStd(miunstd)
    print.semmcci(unstd)
    print.semmcci(miunstd)
    print.semmcci(std)
    print.semmcci(mistd)
    summary.semmcci(unstd)
    summary.semmcci(miunstd)
    summary.semmcci(std)
    summary.semmcci(mistd)
    coef.semmcci(unstd)
    coef.semmcci(miunstd)
    coef.semmcci(std)
    coef.semmcci(mistd)
    vcov.semmcci(unstd)
    vcov.semmcci(miunstd)
    vcov.semmcci(std)
    vcov.semmcci(mistd)
    confint.semmcci(unstd)
    confint.semmcci(miunstd)
    confint.semmcci(std)
    confint.semmcci(mistd)
  },
  R = 100L,
  text = "test-semmcci-mc-methods"
)
