## ---- test-semmcci-zzz-coverage
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)
    semmcci:::.MICombine(
      coefs = list(rep(x = 0, times = 3), rep(x = 0, times = 3)),
      vcovs = list(diag(3), diag(3)),
      M = 2,
      k = 3,
      adj = FALSE
    )
  },
  text = "test-semmcci-zzz-coverage"
)
