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
  a_mat <- matrix(
    data = 0,
    nrow = k,
    ncol = k
  )
  colnames(a_mat) <- rownames(a_mat) <- all_names
  s_mat <- matrix(
    data = 0,
    nrow = k,
    ncol = k
  )
  colnames(s_mat) <- rownames(s_mat) <- all_names
  f_mat <- cbind(
    diag(p),
    matrix(
      data = 0,
      nrow = p,
      ncol = q
    )
  )
  colnames(f_mat) <- all_names
  rownames(f_mat) <- ov_names
  m_mat <- matrix(
    data = 0,
    nrow = 1,
    ncol = p + q
  )
  colnames(m_mat) <- all_names

  # Theta to S
  theta_names <- rownames(lav_mod$theta)
  s_mat[theta_names, theta_names] <- lav_mod$theta

  # Psi to S
  psi_names <- rownames(lav_mod$psi)
  s_mat[psi_names, psi_names] <- lav_mod$psi

  # Lambda to A
  lambda_rnames <- rownames(lav_mod$lambda)
  lambda_cnames <- colnames(lav_mod$lambda)
  a_mat[lambda_rnames, lambda_cnames] <- lav_mod$lambda

  # Beta to A
  if (!is.null(lav_mod$beta)) {
    beta_names <- rownames(lav_mod$beta)
    a_mat[beta_names, beta_names] <- lav_mod$beta
  }

  # Nu to M
  if (!is.null(lav_mod$nu)) {
    m_mat[, rownames(lav_mod$nu)] <- lav_mod$nu
  } else {
    m_mat[, rownames(lav_mod$nu)] <- NA
  }

  # Alpha to M
  if (!is.null(lav_mod$alpha)) {
    m_mat[, rownames(lav_mod$alpha)] <- lav_mod$alpha
  } else {
    m_mat[, rownames(lav_mod$alpha)] <- NA
  }

  list(
    A = a_mat,
    S = s_mat,
    F = f_mat,
    M = m_mat
  )
}
