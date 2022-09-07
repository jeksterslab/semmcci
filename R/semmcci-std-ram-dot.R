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
#' @keywords matrix standardized internal
#' @noRd
.StdRAM <- function(ram_est) {
  mA <- ram_est$A
  mS <- ram_est$S
  iA <- diag(nrow(mA))
  Binv <- solve(iA - mA)
  sdinv <- diag((diag(Binv %*% mS %*% t(Binv)))^(-.5))
  mAz <- sdinv %*% mA %*% solve(sdinv)
  mSz <- sdinv %*% mS %*% sdinv
  colnames(mAz) <- rownames(mAz) <- colnames(mA)
  colnames(mSz) <- rownames(mSz) <- colnames(mS)
  mMz <- ram_est$M
  # TODO: Mz of x = Mz / (SD of x)
  mMz[] <- 0
  list(
    A = mAz,
    S = mSz,
    F = ram_est$F,
    M = mMz
  )
}
