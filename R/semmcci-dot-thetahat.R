#' Extract Parameter Estimates
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object object of class `lavaan`.
#' @importFrom stats complete.cases
#' @keywords internal
#' @noRd
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
