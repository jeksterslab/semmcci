#' Standardized Solution from RAM Matrices
#'
#' Computes the standardized solution from RAM matrices.
#'
#' @details Standardized solution for the means is not yet supported.
#'
#' @author Shu Fai Cheung
#' @param ram_est A list of RAM matrices for one group, with
#'   unstandardized estimates.
#' @return A list of RAM matrices with the standardized
#'   solution.
#'
#' @family Lavaan Helper Functions
#' @keywords lavHelper matrix standardized internal
#' @noRd
.StdRAM <- function(ram_est) {
  a_mat <- ram_est$A
  s_mat <- ram_est$S
  b_inv <- solve(a = diag(nrow = nrow(x = a_mat)) - a_mat)
  # diag(b_inv %*% s_mat %*% t(b_inv)) without forming the full product
  sd_inv <- rowSums(x = (b_inv %*% s_mat) * b_inv)^(-.5)
  a_matz <- a_mat * tcrossprod(x = sd_inv, y = 1 / sd_inv)
  s_matz <- s_mat * tcrossprod(x = sd_inv)
  colnames(x = a_matz) <- rownames(x = a_matz) <- colnames(x = a_mat)
  colnames(x = s_matz) <- rownames(x = s_matz) <- colnames(x = s_mat)
  m_matz <- ram_est$M
  # TODO: Mz of x = Mz / (SD of x)
  m_matz[] <- 0
  list(
    A = a_matz,
    S = s_matz,
    F = ram_est$F,
    M = m_matz
  )
}
