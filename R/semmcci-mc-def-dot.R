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
  if (length(thetahat$def) > 0) {
    def <- function(i) {
      out <- tryCatch(
        {
          object@Model@def.function(
            thetahatstar_orig[
              i,
            ]
          )
        },
        warning = function(w) {
          NA
        },
        error = function(e) {
          NA
        }
      )
      out
    }
    thetahatstar_def <- lapply(
      X = seq_len(
        dim(
          thetahatstar_orig
        )[1]
      ),
      FUN = def
    )
    thetahatstar_def <- do.call(
      what = "rbind",
      args = thetahatstar_def
    )
    thetahatstar <- cbind(
      thetahatstar_orig,
      thetahatstar_def
    )
  } else {
    thetahatstar <- thetahatstar_orig
  }
  # generate equality
  if (length(thetahat$ceq) > 0) {
    ceq <- function(i) {
      out <- object@Model@ceq.function(
        thetahatstar[
          i,
        ]
      )
      names(out) <- paste0(
        thetahat$ceq,
        "_ceq"
      )
      out
    }
    thetahatstar_ceq <- lapply(
      X = seq_len(
        dim(
          thetahatstar
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
  if (length(thetahat$cin) > 0) {
    cin <- function(i) {
      out <- object@Model@cin.function(
        thetahatstar[
          i,
        ]
      )
      names(out) <- paste0(
        thetahat$cin,
        "_cin"
      )
      out
    }
    thetahatstar_cin <- lapply(
      X = seq_len(
        dim(
          thetahatstar
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
  if (length(thetahat$fixed) > 0) {
    fixed <- matrix(
      NA,
      ncol = length(
        thetahat$fixed
      ),
      nrow = dim(
        thetahatstar
      )[1]
    )
    colnames(
      fixed
    ) <- thetahat$fixed
    for (i in seq_len(dim(fixed)[2])) {
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
