#' Test for a Positive Definite Matrix (Internal)
#'
#' Returns `TRUE` if input
#' is a positive definite matrix,
#' and `FALSE` otherwise.
#'
#' A
#' \eqn{k \times k}
#' symmetric matrix
#' \eqn{\boldsymbol{A}}
#' is positive definite
#' if all of its eigenvalues are positive.
#' Eigenvalues above `tol` times \eqn{\left| \lambda_{1} \right|},
#' where \eqn{\lambda_{1}} is the largest eigenvalue, pass.
#' Zero eigenvalues fail.
#' No input checks: see `TestPositiveDefinite()`.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param eigen List.
#'   Output of [eigen()], with eigenvalues in decreasing order.
#' @param tol Numeric.
#'   Tolerance.
#'
#' @references
#'   [Wikipedia: Definite matrix](https://en.wikipedia.org/wiki/Definite_matrix)
#'
#' @return Logical.
#'
#' @examples
#' .TestPositiveDefinite(eigen = eigen(x = diag(2), symmetric = TRUE))
#'
#' @family Linear Algebra Functions
#' @keywords linearAlgebra test internal
#' @noRd
.TestPositiveDefinite <- function(eigen,
                                  tol = 1e-06) {
  all(
    eigen$values > tol * abs(
      x = eigen$values[1L]
    )
  )
}
