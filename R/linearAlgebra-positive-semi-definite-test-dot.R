#' Test for a Positive Semidefinite Matrix from Eigenvalues (Internal)
#'
#' Returns `TRUE` if the eigenvalues
#' belong to a positive semidefinite matrix,
#' and `FALSE` otherwise.
#'
#' A
#' \eqn{k \times k}
#' symmetric matrix
#' \eqn{\boldsymbol{A}}
#' is positive semidefinite
#' if none of its eigenvalues is negative.
#' Eigenvalues at or above `-tol` times \eqn{\left| \lambda_{1} \right|},
#' where \eqn{\lambda_{1}} is the largest eigenvalue, pass.
#' Zero eigenvalues pass.
#' No input checks: see `TestPositiveSemiDefinite()`.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param eigen List.
#'   Output of [eigen()] for a symmetric matrix.
#' @param tol Numeric.
#'   Tolerance.
#'
#' @references
#'   [Wikipedia: Definite matrix](https://en.wikipedia.org/wiki/Definite_matrix)
#'
#' @return Logical.
#'
#' @examples
#' .TestPositiveSemiDefinite(
#'   eigen = eigen(x = matrix(data = 1, nrow = 2, ncol = 2))
#' )
#'
#' @family Linear Algebra Functions
#' @keywords linearAlgebra test internal
#' @noRd
.TestPositiveSemiDefinite <- function(eigen,
                                      tol = 1e-06) {
  all(
    eigen$values >= -tol * abs(
      x = eigen$values[1L]
    )
  )
}
