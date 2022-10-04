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
#' @param eigen output of the `eigen()` function.
#' @param tol Numeric.
#'   Tolerance.
#'
#' @references
#'   [Wikipedia: Definite matrix](https://en.wikipedia.org/wiki/Definite_matrix)
#'
#' @return Logical.
#'
#' @examples
#' # TRUE
#' .TestPositiveDefinite(eigen(diag(2)))
#'
#' # FALSE
#' .TestPositiveDefinite(
#'   eigen(
#'     matrix(
#'       data = c(1, 2, 3, 2, 4, 5, 3, 5, 6),
#'       ncol = 3
#'     )
#'   )
#' )
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
