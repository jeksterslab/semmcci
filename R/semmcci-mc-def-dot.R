#' MC for Define Parameters
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object object of class `lavaan`.
#' @param thetahat Output of [.ThetaHat()].
#' @param thetahatstar_orig Numeric matrix.
#'   Monte Carlo estimates of free parameters.
#'
#' @return Returns a matrix with free and defined Monte Carlo estimates.
#'
#' @family Monte Carlo in Structural Equation Modeling Functions
#' @keywords semmcci mc def internal
#' @noRd
.MCDef <- function(object,
                   thetahat,
                   thetahatstar_orig) {
  # generate defined parameters
  if (length(x = thetahat$def) > 0) {
    thetahatstar_def <- .MCApply(
      X = thetahatstar_orig,
      FUN = object@Model@def.function,
      p = length(x = thetahat$def)
    )
    thetahatstar <- cbind(
      thetahatstar_orig,
      thetahatstar_def
    )
  } else {
    thetahatstar <- thetahatstar_orig
  }
  # generate equality
  if (length(x = thetahat$ceq) > 0) {
    ceq <- function(i) {
      out <- object@Model@ceq.function(
        thetahatstar[
          i,
        ]
      )
      names(x = out) <- paste0(
        thetahat$ceq,
        "_ceq"
      )
      out
    }
    thetahatstar_ceq <- lapply(
      X = seq_len(
        length.out = dim(
          x = thetahatstar
        )[1]
      ),
      FUN = ceq
    )
    thetahatstar_ceq <- do.call(
      what = "rbind",
      args = thetahatstar_ceq
    )
    thetahatstar <- cbind(
      thetahatstar,
      thetahatstar_ceq
    )
  }
  # generate inequality
  if (length(x = thetahat$cin) > 0) {
    cin <- function(i) {
      out <- object@Model@cin.function(
        thetahatstar[
          i,
        ]
      )
      names(x = out) <- paste0(
        thetahat$cin,
        "_cin"
      )
      out
    }
    thetahatstar_cin <- lapply(
      X = seq_len(
        length.out = dim(
          x = thetahatstar
        )[1]
      ),
      FUN = cin
    )
    thetahatstar_cin <- do.call(
      what = "rbind",
      args = thetahatstar_cin
    )
    thetahatstar <- cbind(
      thetahatstar,
      thetahatstar_cin
    )
  }
  # generate fixed
  if (length(x = thetahat$fixed) > 0) {
    fixed <- matrix(
      data = NA,
      ncol = length(
        x = thetahat$fixed
      ),
      nrow = dim(
        x = thetahatstar
      )[1]
    )
    colnames(
      x = fixed
    ) <- thetahat$fixed
    for (i in seq_len(length.out = dim(x = fixed)[2])) {
      fixed[
        ,
        i
      ] <- thetahat$est[
        thetahat$fixed[[i]]
      ]
    }
    thetahatstar <- cbind(
      thetahatstar,
      fixed
    )
  }
  # rearrange
  thetahatstar[
    ,
    thetahat$par_names
  ]
}
