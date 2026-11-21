#' User Parameter Vector from a lavaan Model
#'
#' Extracts the user parameters (including defined parameters,
#' equality constraints, and inequality constraints)
#' from the matrices of a `lavaan` model.
#' The result equals
#' `lavaan::lav_model_get_parameters(lavmodel, type = "user", extra = TRUE)`,
#' without the overhead of the generic function,
#' which matters when it is called once per Monte Carlo draw.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param lavmodel An object of class `lavModel`.
#' @return A numeric vector.
#'
#' @family Lavaan Helper Functions
#' @keywords lavHelper internal
#' @noRd
.GetUser <- function(lavmodel) {
  glist <- lavmodel@GLIST
  x <- numeric(length = lavmodel@nx.user)
  for (mm in seq_along(along.with = glist)) {
    x[lavmodel@x.user.idx[[mm]]] <- glist[[mm]][lavmodel@m.user.idx[[mm]]]
  }
  if (
    length(x = lavmodel@x.def.idx) +
      length(x = lavmodel@x.ceq.idx) +
      length(x = lavmodel@x.cin.idx) > 0L
  ) {
    x_free <- numeric(length = lavmodel@nx.free)
    for (mm in seq_along(along.with = glist)) {
      x_free[lavmodel@x.free.idx[[mm]]] <- glist[[mm]][
        lavmodel@m.free.idx[[mm]]
      ]
    }
    if (length(x = lavmodel@x.def.idx) > 0L) {
      x[lavmodel@x.def.idx] <- lavmodel@def.function(x_free)
    }
    if (length(x = lavmodel@x.ceq.idx) > 0L) {
      x[lavmodel@x.ceq.idx] <- lavmodel@ceq.function(x_free)
    }
    if (length(x = lavmodel@x.cin.idx) > 0L) {
      tmp <- lavmodel@cin.function(x_free)
      bound_idx <- attr(x = tmp, which = "bound.idx")
      if (length(x = bound_idx) > 0L) {
        tmp <- tmp[-bound_idx]
      }
      x[lavmodel@x.cin.idx] <- tmp
    }
  }
  x
}
