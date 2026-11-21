#' Apply a Function to the Rows of a Matrix of Monte Carlo Draws
#'
#' Applies `FUN` to every row of `X` and binds the results by row.
#' A warning or an error in `FUN` yields a row of `NA` for that draw.
#' The whole loop first runs under a single `tryCatch()`,
#' which is much faster than one `tryCatch()` per draw.
#' If any draw signals a warning or an error,
#' the draws are redone one at a time,
#' so the result is the same as with a `tryCatch()` per draw.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param X Numeric matrix. Each row is a Monte Carlo draw.
#' @param FUN Function of one numeric vector
#'   that returns a numeric vector of fixed length.
#' @param p Integer. Length of the output of `FUN`
#'   used for the `NA` rows.
#'   If `NULL`, it is the length of `FUN(X[1, ])`.
#'
#' @return Returns a matrix with `nrow(X)` rows.
#'
#' @family Monte Carlo in Structural Equation Modeling Functions
#' @keywords semmcci mc internal
#' @noRd
.MCApply <- function(X,
                     FUN,
                     p = NULL) {
  n <- dim(x = X)[1]
  fast <- tryCatch(
    expr = {
      first <- FUN(X[1, ])
      out <- vapply(
        X = seq_len(length.out = n),
        FUN = function(i) {
          FUN(X[i, ])
        },
        FUN.VALUE = first
      )
      if (is.matrix(x = out)) {
        t(x = out)
      } else {
        matrix(
          data = out,
          ncol = 1,
          dimnames = list(NULL, names(x = first))
        )
      }
    },
    warning = function(w) {
      NULL
    },
    error = function(e) {
      NULL
    }
  )
  if (!is.null(x = fast)) {
    fast
  } else {
    do.call(
      what = "rbind",
      args = lapply(
        X = seq_len(length.out = n),
        FUN = function(i) {
          tryCatch(
            expr = FUN(X[i, ]),
            warning = function(w) {
              rep(
                x = NA,
                times = p
              )
            },
            error = function(e) {
              rep(
                x = NA,
                times = p
              )
            }
          )
        }
      )
    )
  }
}
