#' lavaan to RAM matrices
#'
#' Converts a list of lavaan matrices to a list of RAM matrices.
#'
#' @author Shu Fai Cheung
#' @param lav_mod A list of lavaan model matrices for one group.
#' @return A list of A, S, F, and M RAM matrices.
#' @keywords matrix internal
#' @noRd
.Lav2RAM <- function(lav_mod) {
  ov_names <- rownames(lav_mod$theta)
  lv_names <- rownames(lav_mod$psi)
  lv_names <- lv_names[!(lv_names %in% ov_names)]
  all_names <- c(ov_names, lv_names)
  p <- length(ov_names)
  q <- length(lv_names)
  k <- length(all_names)

  # Initialize the matrices
  mA <- matrix(
    data = 0,
    nrow = k,
    ncol = k
  )
  colnames(mA) <- rownames(mA) <- all_names
  mS <- matrix(
    data = 0,
    nrow = k,
    ncol = k
  )
  colnames(mS) <- rownames(mS) <- all_names
  mF <- cbind(
    diag(p),
    matrix(
      data = 0,
      nrow = p,
      ncol = q
    )
  )
  colnames(mF) <- all_names
  rownames(mF) <- ov_names
  mM <- matrix(
    data = 0,
    nrow = 1,
    ncol = p + q
  )
  colnames(mM) <- all_names

  # Theta to S
  theta_names <- rownames(lav_mod$theta)
  mS[theta_names, theta_names] <- lav_mod$theta

  # Psi to S
  psi_names <- rownames(lav_mod$psi)
  mS[psi_names, psi_names] <- lav_mod$psi

  # Lambda to A
  lambda_rnames <- rownames(lav_mod$lambda)
  lambda_cnames <- colnames(lav_mod$lambda)
  mA[lambda_rnames, lambda_cnames] <- lav_mod$lambda

  # Beta to A
  if (!is.null(lav_mod$beta)) {
    beta_names <- rownames(lav_mod$beta)
    mA[beta_names, beta_names] <- lav_mod$beta
  }

  # Nu to M
  if (!is.null(lav_mod$nu)) {
    mM[, rownames(lav_mod$nu)] <- lav_mod$nu
  } else {
    mM[, rownames(lav_mod$nu)] <- NA
  }

  # Alpha to M
  if (!is.null(lav_mod$alpha)) {
    mM[, rownames(lav_mod$alpha)] <- lav_mod$alpha
  } else {
    mM[, rownames(lav_mod$alpha)] <- NA
  }

  list(
    A = mA,
    S = mS,
    F = mF,
    M = mM
  )
}
