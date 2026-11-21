#' Extract Parameter Estimates
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object object of class `lavaan`.
#' @param est Numeric vector.
#'   If not `NULL`, the parameter estimates are updated.
#'   using the supplied vector.
#'   This is used to update the values in the parameter vector
#'   using new estimates.
#' @return Returns a list with the following elements
#' \describe{
#'   \item{est}{Parameter estimates.}
#'   \item{par_names}{Parameter names.}
#'   \item{def}{Defined parameters.}
#'   \item{ceq}{Equality constraints.}
#'   \item{cin}{Inequality constraints.}
#'   \item{fixed}{Fixed parameters.}
#' }
#'
#' @family Lavaan Helper Functions
#' @keywords lavHelper parameters internal
#' @noRd
.ThetaHat <- function(object,
                      est = NULL) {
  # extract all estimates including fixed parameters
  thetahat_names_free <- names(
    x = lavaan::coef(object = object)
  )
  thetahat_eq <- paste0(
    object@ParTable$lhs,
    object@ParTable$op,
    object@ParTable$rhs
  )
  thetahat_names <- thetahat_eq
  label <- object@ParTable$label
  thetahat_names <- ifelse(
    test = label == "",
    yes = thetahat_names,
    no = label
  )
  for (i in seq_along(along.with = object@ParTable$free)) {
    if (object@ParTable$free[i] > 0) {
      thetahat_names[i] <- thetahat_names_free[
        object@ParTable$free[i]
      ]
    }
  }
  if (is.null(x = est)) {
    # extract estimates from parameter table
    thetahat_est <- object@ParTable$est
  } else {
    # update with supplied values
    thetahat_est <- est
  }
  .ThetaHatDef(
    object = object,
    thetahat_est = thetahat_est,
    thetahat_names_free = thetahat_names_free,
    thetahat_names = thetahat_names
  )
}
