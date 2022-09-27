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
    std <- MCStd(unstd)
    print.semmcci(unstd)
    print.semmccistd(std)
    summary.semmcci(unstd)
    summary.semmccistd(std)
    coef.semmcci(unstd)
    coef.semmccistd(std)
    vcov.semmcci(unstd)
    vcov.semmccistd(std)
    confint.semmcci(unstd)
    confint.semmccistd(std)
  },
  R = 100L,
  text = "test-semmcci-mc-methods"
)
