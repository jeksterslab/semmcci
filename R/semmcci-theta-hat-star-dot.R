#' Generate Sampling Distribution of Parameter Estimates
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @details
#' `Step 1`: Generate `2 + R + R * .50` random variates.
#' `Step 2`: Drop cases with negative variances from the result of `Step 1` and store the resulting matrix.
#' `Step 3`: Check if the number of rows in the result of `Step 2` is greater than or equal to `R`.
#'   If `TRUE`, return the `1:R` rows of the matrix and exit the program.
#'   If `FALSE`, start a `for` loop from `1` to `max_iter`.
#'     `Loop Step 1`: Generate `2 + n + n * .50` random variates where `n` is `R` minus the number of rows of the result of `Step 2`.
#'     `Loop Step 2`: Drop cases with negative variances from the result of `Loop Step 1` and store the resulting matrix.
#'     `Loop Step 3`: Check if the number of rows in the result of `Loop Step 2` is greater than or equal to `n`.
#'       If `TRUE`, return the `1:n` rows of the matrix and exit the loop.
#'       If `FALSE,
#'         update `n` with `n` minus the number of rows of the result of `Loop Step 2`,
#'         increment the iteration number, and
#'         loop back to `Loop Step 1`.
#'         A successful run assumes that the loop arrives at `Loop Step 3` with a `TRUE` result on or before `max_iter` is reached.
#'         If `Loop Step 3` returns `FALSE` at `max_iter`, the program returns an error.
#' `Step 4`: If `Step 3` is `FALSE`, combine the results from the outer and inner processes.
#' The number of rows of the resulting matrix should be equal to `R`.
#'
#' @param object object of class `lavaan`.
#' @param R Positive integer.
#'   Number of Monte Carlo replications.
#' @param max_iter Positive integer.
#'   Maximum iteration to replace negative variances.
#' @param neg_var Logical.
#'   Default is `TRUE`.
#'   If `TRUE`, free variances less than or equal to zero are allowed.
#'   If `FALSE`, generate sampling distribution without free variances less than or equal to zero.
#' @return Returns a matrix of parameter estimates.
#' @importFrom stats complete.cases
#' @keywords parameters internal
#' @noRd
.ThetaHatStar <- function(object,
                          R,
                          max_iter = 100,
                          neg_var = FALSE) {
  mu <- lavaan::coef(object)
  Sigma <- lavaan::vcov(object)
  if (neg_var) {
    return(
      MASS::mvrnorm(
        n = as.integer(R),
        mu = mu,
        Sigma = Sigma
      )
    )
  } else {
    # outer --------------------------------------------------------------------
    idx <- which(
      object@ParTable$op[
        object@ParTable$free > 0
      ] == "~~"
    )
    output <- .ThetaHatStarVars(
      x = MASS::mvrnorm(
        n = as.integer(2 + R + R * .50),
        mu = mu,
        Sigma = Sigma
      ),
      idx = idx
    )
    n <- dim(output)[1]
    if (n >= R) {
      return(output[1:R, , drop = FALSE])
    } else {
      # inner loop -------------------------------------------------------------
      iter <- 0
      n <- R - n
      output_list <- vector(
        mode = "list",
        length = max_iter
      )
      for (i in 1:max_iter) {
        iter <- iter + 1
        output_i <- .ThetaHatStarVars(
          x = MASS::mvrnorm(
            n = as.integer(2 + n + n * .50),
            mu = mu,
            Sigma = Sigma
          ),
          idx = idx
        )
        n_i <- dim(output_i)[1]
        if (n_i >= n) {
          output_list[[i]] <- output_i[1:n, , drop = FALSE]
          break
        } else {
          output_list[[i]] <- output_i
          n <- n - n_i
          if (iter == max_iter) {
            stop(
              "Maximum iterations reached. Too many negative variances generated."
            )
          }
        }
      }
      return(
        output <- rbind(
          output,
          do.call(
            what = "rbind",
            args = output_list[!sapply(X = output_list, FUN = is.null)]
          )
        )
      )
    }
  }
}
