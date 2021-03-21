.pcci <- function(thetahatstar,
                  thetahat,
                  alpha = c(0.001, 0.01, 0.05)) {
  thetahatstar <- as.vector(thetahatstar)
  thetahatstar <- thetahatstar[stats::complete.cases(thetahatstar)]
  alpha <- sort(alpha)
  prob_ll <- alpha / 2
  prob_ul <- rev(1 - prob_ll)
  probs <- c(prob_ll, prob_ul)
  ci <- stats::quantile(
    x = thetahatstar,
    probs = probs
  )
  out <- c(
    thetahat,
    stats::sd(thetahatstar),
    length(thetahatstar),
    ci
  )
  names(out) <- c(
    "est",
    "se",
    "R",
    paste0(
      "ci_",
      probs * 100
    )
  )
  return(out)
}

.thetahat <- function(object) {
  # extract all estimates including fixed parameters
  thetahat.names <- paste0(
    object@ParTable$lhs,
    object@ParTable$op,
    object@ParTable$rhs
  )
  thetahat.labels <- thetahat.names
  label <- object@ParTable$label
  thetahat.labels <- ifelse(
    label == "",
    yes = thetahat.labels,
    no = label
  )
  thetahat.est <- object@ParTable$est
  thetahat.fixed <- thetahat.def <- thetahat.cin <- thetahat.ceq <- rep(
    x = NA,
    times = length(thetahat.est)
  )
  for (i in seq_along(thetahat.est)) {
    if (object@ParTable$op[i] == "==") {
      thetahat.labels[i] <- paste0(
        object@ParTable$lhs[i],
        "_ceq"
      )
      thetahat.ceq[i] <- object@ParTable$lhs[i]
    }
    if (object@ParTable$op[i] %in% c(">", "<", ">=", "<=")) {
      thetahat.labels[i] <- paste0(
        object@ParTable$lhs[i],
        "_cin"
      )
      thetahat.cin[i] <- object@ParTable$lhs[i]
    }
    if (object@ParTable$op[i] == ":=") {
      thetahat.def[i] <- object@ParTable$lhs[i]
    }
    if (object@ParTable$free[i] == 0) {
      fixed <- TRUE
      if (object@ParTable$op[i] %in% c(":=", "==", ">", "<", ">=", "<=")) {
        fixed <- FALSE
      }
      if (fixed) {
        thetahat.fixed[i] <- thetahat.labels[i]
      }
    }
  }
  names(thetahat.est) <- thetahat.labels
  def <- thetahat.def[stats::complete.cases(thetahat.def)]
  ceq <- thetahat.ceq[stats::complete.cases(thetahat.ceq)]
  cin <- thetahat.cin[stats::complete.cases(thetahat.cin)]
  fixed <- thetahat.fixed[stats::complete.cases(thetahat.fixed)]
  list(
    est = thetahat.est,
    names = thetahat.names,
    labels = thetahat.labels,
    def = def,
    ceq = ceq,
    cin = cin,
    fixed = fixed
  )
}
