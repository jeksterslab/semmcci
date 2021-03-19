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
  thetahat_names <- paste0(
    object@ParTable$lhs,
    object@ParTable$op,
    object@ParTable$rhs
  )
  label <- object@ParTable$label
  thetahat_names <- ifelse(
    label == "",
    yes = thetahat_names,
    no = label
  )
  thetahat <- object@ParTable$est
  names(thetahat) <- thetahat_names
  return(thetahat)
}
