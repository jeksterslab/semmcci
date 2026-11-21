#' Parameter Estimates of a lavaan Object (Helper)
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `lavaan`.
#' @param thetahat_est Numeric vector.
#'   Parameter estimates (one for each row of the parameter table).
#' @param thetahat_names_free Character vector.
#'   Names of the free parameters.
#' @param thetahat_names Character vector.
#'   Names of all the parameters.
#'
#' @family Lavaan Helper Functions
#' @keywords lavHelper parameters internal
#' @noRd
.ThetaHatDef <- function(object,
                         thetahat_est,
                         thetahat_names_free,
                         thetahat_names) {
  thetahat_fixed <- thetahat_def <- thetahat_cin <- thetahat_ceq <- rep(
    x = NA,
    times = length(x = thetahat_est)
  )
  for (i in seq_along(along.with = thetahat_est)) {
    if (object@ParTable$op[i] == "==") {
      thetahat_names[i] <- paste0(
        object@ParTable$lhs[i],
        "_ceq"
      )
      thetahat_ceq[i] <- object@ParTable$lhs[i]
    }
    if (
      object@ParTable$op[i] %in% c(
        ">",
        "<",
        ">=",
        "<="
      )
    ) {
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
      if (
        object@ParTable$op[i] %in% c(
          ":=",
          "==",
          ">",
          "<",
          ">=",
          "<="
        )
      ) {
        fixed <- FALSE
      }
      if (fixed) {
        thetahat_fixed[i] <- thetahat_names[i]
      }
    }
  }
  names(
    x = thetahat_est
  ) <- thetahat_names
  list(
    est = thetahat_est,
    par_names = thetahat_names,
    def = thetahat_def[
      stats::complete.cases(thetahat_def)
    ],
    ceq = thetahat_ceq[
      stats::complete.cases(thetahat_ceq)
    ],
    cin = thetahat_cin[
      stats::complete.cases(thetahat_cin)
    ],
    fixed = thetahat_fixed[
      stats::complete.cases(thetahat_fixed)
    ],
    free = thetahat_names_free
  )
}
