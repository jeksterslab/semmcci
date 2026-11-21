#' RAM to lavaan matrices
#'
#' Converts a list of RAM matrices to a list of `lavaan` matrices.
#'
#' @author Shu Fai Cheung
#' @param ram A list of RAM model matrices for one group.
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
  ov_names <- rownames(x = lav_mod$theta)
  lv_names <- rownames(x = lav_mod$psi)
  lv_names <- lv_names[!(lv_names %in% ov_names)]

  a_mat <- ram$A
  # observed variables that lavaan also lists as latent (none by default)
  lambda1 <- character(length = 0)

  if (!is.null(x = lav_mod$lambda)) {
    # A to lambda
    lav_mod$lambda[] <- 0
    lambda_rnames <- rownames(x = lav_mod$lambda)
    lambda_cnames <- colnames(x = lav_mod$lambda)
    lav_mod$lambda <- a_mat[
      lambda_rnames,
      lambda_cnames,
      drop = FALSE
    ]
    lambda1 <- lambda_rnames[lambda_rnames %in% lambda_cnames]
    lav_mod$lambda[lambda1, ] <- 0
    lav_mod$lambda[lambda1, lambda1] <- 1
  }

  if (!is.null(x = lav_mod$beta)) {
    # A to beta
    lav_mod$beta[] <- 0
    beta_names <- rownames(x = lav_mod$beta)
    lav_mod$beta <- a_mat[beta_names, beta_names, drop = FALSE]
  }

  s_mat <- ram$S

  if (!is.null(x = lav_mod$theta)) {
    # S to theta
    lav_mod$theta[] <- 0
    theta_names <- rownames(x = lav_mod$theta)
    lav_mod$theta <- s_mat[theta_names, theta_names, drop = FALSE]
    if (standardized) {
      tmp <- diag(x = lav_mod$theta)
      lav_mod$theta <- stats::cov2cor(V = lav_mod$theta)
      diag(x = lav_mod$theta) <- tmp
    }
    lav_mod$theta[lambda1, lambda1] <- 0
  }
  if (!is.null(x = lav_mod$psi)) {
    # S to psi
    lav_mod$psi[] <- 0
    psi_names <- rownames(x = lav_mod$psi)
    lav_mod$psi <- s_mat[psi_names, psi_names, drop = FALSE]
    if (standardized) {
      tmp <- diag(x = lav_mod$psi)
      lav_mod$psi <- stats::cov2cor(V = lav_mod$psi)
      diag(x = lav_mod$psi) <- tmp
    }
  }

  if (!is.null(x = lav_mod$nu)) {
    # M to nu
    lav_mod$nu[] <- 0
    nu_names <- rownames(x = lav_mod$nu)
    lav_mod$nu[] <- t(x = ram$M[, nu_names, drop = FALSE])
    lav_mod$nu[lambda1, ] <- 0
  }

  if (!is.null(x = lav_mod$alpha)) {
    # M to alpha
    lav_mod$alpha[] <- 0
    alpha_names <- rownames(x = lav_mod$alpha)
    lav_mod$alpha[] <- t(x = ram$M[, alpha_names, drop = FALSE])
  }

  lav_mod
}
