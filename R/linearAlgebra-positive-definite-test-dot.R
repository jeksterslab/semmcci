#' Test for a Positive Definite Matrix
#'
#' Returns `TRUE` if input
#' is a positive definite matrix,
#' and `FALSE` otherwise.
#'
#' A
#' \eqn{k \times k}
#' symmetric matrix
#' \eqn{\mathbf{A}}
#' is positive definite
#' if all of its eigenvalues are positive.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param eigen output of the [eigen()] function.
#' @param tol Numeric.
#'   Tolerance.
#'
#' @references
#'   [Wikipedia: Definite matrix](https://en.wikipedia.org/wiki/Definite_matrix)
#'
#' @return Logical.
#'
#' @family Linear Algebra Functions
#' @keywords linearAlgebra test internal
#' @noRd
.TestPositiveDefinite <- function(eigen,
                                  tol = 1e-06) {
  return(
    all(
      eigen$values >= -tol * abs(
        eigen$values[1L]
      )
    )
  )
}
