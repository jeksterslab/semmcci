#' Extract Parameter Estimates
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object object of class `lavaan`.
#' @return Returns a list with the following elements
#' \describe{
#'   \item{`est`}{Parameter estimates.}
#'   \item{`par_names`}{Parameter names.}
#'   \item{`def`}{Defined parameters.}
#'   \item{`ceq`}{Equality constraints.}
#'   \item{`cin`}{Inequality constraints.}
#'   \item{`fixed`}{Fixed parameters.}
#' }
#' @importFrom stats complete.cases
#' @keywords parameters internal
#' @noRd
.ThetaHat <- function(object) {
  # extract all estimates including fixed parameters
  thetahat_names_free <- names(lavaan::coef(object))
  thetahat_eq <- paste0(
    object@ParTable$lhs,
    object@ParTable$op,
    object@ParTable$rhs
  )
  thetahat_names <- thetahat_eq
  label <- object@ParTable$label
  thetahat_names <- ifelse(
    label == "",
    yes = thetahat_names,
    no = label
  )
  for (i in seq_along(object@ParTable$free)) {
    if (object@ParTable$free[i] > 0) {
      thetahat_names[i] <- thetahat_names_free[object@ParTable$free[i]]
    }
  }
  thetahat_est <- object@ParTable$est
  thetahat_fixed <- thetahat_def <- thetahat_cin <- thetahat_ceq <- rep(
    x = NA,
    times = length(thetahat_est)
  )
  for (i in seq_along(thetahat_est)) {
    if (object@ParTable$op[i] == "==") {
      thetahat_names[i] <- paste0(
        object@ParTable$lhs[i],
        "_ceq"
      )
      thetahat_ceq[i] <- object@ParTable$lhs[i]
    }
    if (object@ParTable$op[i] %in% c(">", "<", ">=", "<=")) {
      thetahat_names[i] <- paste0(
        object@ParTable$lhs[i],
        "_cin"
      )
      thetahat_cin[i] <- object@ParTable$lhs[i]
    }
    if (object@ParTable$op[i] == ":=") {
      thetahat_def[i] <- object@ParTable$lhs[i]
    }
    if (object@ParTable$free[i] == 0) {
      fixed <- TRUE
      if (object@ParTable$op[i] %in% c(":=", "==", ">", "<", ">=", "<=")) {
        fixed <- FALSE
      }
      if (fixed) {
        thetahat_fixed[i] <- thetahat_names[i]
      }
    }
  }
  names(thetahat_est) <- thetahat_names
  def <- thetahat_def[stats::complete.cases(thetahat_def)]
  ceq <- thetahat_ceq[stats::complete.cases(thetahat_ceq)]
  cin <- thetahat_cin[stats::complete.cases(thetahat_cin)]
  fixed <- thetahat_fixed[stats::complete.cases(thetahat_fixed)]
  return(
    list(
      est = thetahat_est,
      par_names = thetahat_names,
      def = def,
      ceq = ceq,
      cin = cin,
      fixed = fixed
    )
  )
}
