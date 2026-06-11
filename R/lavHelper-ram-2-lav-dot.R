#' RAM to lavaan matrices
#'
#' Converts a list of RAM matrices to a list of `lavaan` matrices.
#'
#' @author Shu Fai Cheung
#' @return A list of lavaan matrices.
#' @param ram_mod A list of RAM model matrices for one group.
#' @param lav_mod A list of `lavaan` model matrices for one
#'   group. They will be used as holders of the output.
#' @param standardized If `TRUE`, the `psi` and `beta` matrices
#'  contain the correlations. Default is `FALSE`.
#' @return A list of `lavaan` model matrices for one group.
#'
#' @family Lavaan Helper Functions
#' @keywords lavHelper matrix internal
#' @noRd
.RAM2Lav <- function(ram,
                     lav_mod,
                     standardized = FALSE) {
  ov_names <- rownames(lav_mod$theta)
  lv_names <- rownames(lav_mod$psi)
  lv_names <- lv_names[!(lv_names %in% ov_names)]

  a_mat <- ram$A

  if (!is.null(lav_mod$lambda)) {
    # A to lambda
    lav_mod$lambda[] <- 0
    lambda_rnames <- rownames(lav_mod$lambda)
    lambda_cnames <- colnames(lav_mod$lambda)
    lav_mod$lambda <- a_mat[
      lambda_rnames,
      lambda_cnames
    ]
    lambda1 <- lambda_rnames[lambda_rnames %in% lambda_cnames]
    lav_mod$lambda[lambda1, ] <- 0
    lav_mod$lambda[lambda1, lambda1] <- 1
  }

  if (!is.null(lav_mod$beta)) {
    # A to beta
    lav_mod$beta[] <- 0
    beta_names <- rownames(lav_mod$beta)
    lav_mod$beta <- a_mat[beta_names, beta_names]
  }

  s_mat <- ram$S

  if (!is.null(lav_mod$theta)) {
    # S to theta
    lav_mod$theta[] <- 0
    theta_names <- rownames(lav_mod$theta)
    lav_mod$theta <- s_mat[theta_names, theta_names]
    if (standardized) {
      tmp <- diag(lav_mod$theta)
      lav_mod$theta <- stats::cov2cor(lav_mod$theta)
      diag(lav_mod$theta) <- tmp
    }
    lav_mod$theta[lambda1, lambda1] <- 0
  }
  if (!is.null(lav_mod$psi)) {
    # S to psi
    lav_mod$psi[] <- 0
    psi_names <- rownames(lav_mod$psi)
    lav_mod$psi <- s_mat[psi_names, psi_names]
    if (standardized) {
      tmp <- diag(lav_mod$psi)
      lav_mod$psi <- stats::cov2cor(lav_mod$psi)
      diag(lav_mod$psi) <- tmp
    }
  }

  if (!is.null(lav_mod$nu)) {
    # M to nu
    lav_mod$nu[] <- 0
    nu_names <- rownames(lav_mod$nu)
    lav_mod$nu[] <- t(ram$M[, nu_names, drop = FALSE])
    lav_mod$nu[lambda1] <- 0
  }

  if (!is.null(lav_mod$alpha)) {
    # M to alpha
    lav_mod$alpha[] <- 0
    alpha_names <- rownames(lav_mod$alpha)
    lav_mod$alpha[] <- t(ram$M[, alpha_names, drop = FALSE])
  }

  lav_mod
}
