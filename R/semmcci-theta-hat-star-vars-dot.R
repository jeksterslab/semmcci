#' Remove Cases with Negative Variances
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric matrix.
#'   Sampling distribution of parameter estimates.
#' @param idx Vector of positive integers.
#'   Column index of variances in `x`.
#' @return Returns a matrix of parameter estimates.
#' @importFrom stats complete.cases
#' @keywords parameters internal
#' @noRd
.ThetaHatStarVars <- function(x,
                              idx) {
  for (j in seq_along(idx)) {
    for (i in seq_len(dim(x)[2])) {
      if (i == idx[j]) {
        x[, i] <- ifelse(
          test = x[, i] <= 0,
          yes = NA,
          no = x[, i]
        )
      }
    }
  }
  return(
    x[stats::complete.cases(x), , drop = FALSE]
  )
}
