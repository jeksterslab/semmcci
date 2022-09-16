## ---- test-semmcci-mc-print
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
    print.semmcci(unstd)
    print.semmcci_std(MCStd(unstd))
  },
  R = 100L,
  text = "test-semmcci-mc-print"
)
