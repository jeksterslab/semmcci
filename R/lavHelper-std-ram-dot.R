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
  iden <- diag(nrow(a_mat))
  b_inv <- solve(iden - a_mat)
  sdinv <- diag(
    (
      diag(
        b_inv %*% s_mat %*% t(
          b_inv
        )
      )
    )^(-.5)
  )
  a_matz <- (
    sdinv %*% a_mat %*% solve(sdinv)
  )
  s_matz <- (
    sdinv %*% s_mat %*% sdinv
  )
  colnames(a_matz) <- rownames(a_matz) <- colnames(a_mat)
  colnames(s_matz) <- rownames(s_matz) <- colnames(s_mat)
  m_matz <- ram_est$M
  # TODO: Mz of x = Mz / (SD of x)
  m_matz[] <- 0
  return(
    list(
      A = a_matz,
      S = s_matz,
      F = ram_est$F,
      M = m_matz
    )
  )
}
