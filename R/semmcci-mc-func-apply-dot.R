#' Apply a Function to the Monte Carlo Draws of `MCFunc()`
#'
#' Applies `FUN` to every element of `X`.
#' It runs serially when `ncores` is `NULL` or 1.
#' Otherwise, it forks (`parallel::mclapply()`) on Darwin and Linux
#' and uses a socket cluster (`parallel::parLapply()`) elsewhere.
#' The number of cores is capped at `parallel::detectCores()`.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param X List or data frame. Each element is a Monte Carlo draw.
#' @param FUN Function.
#' @param ... Arguments passed to `FUN`.
#' @param ncores Integer or `NULL`. Number of cores.
#'
#' @return Returns a list with one element per element of `X`.
#'
#' @family Monte Carlo in Structural Equation Modeling Functions
#' @keywords semmcci mc internal
#' @noRd
.MCFuncApply <- function(X,
                         FUN,
                         ...,
                         ncores = NULL) {
  par <- FALSE
  if (!is.null(x = ncores)) {
    ncores <- as.integer(x = ncores)
    if (ncores > 1) {
      par <- TRUE
    }
  }
  if (!par) {
    return_value <- lapply(
      X = X,
      FUN = FUN,
      ...
    )
  } else {
    available_cores <- parallel::detectCores()
    if (ncores >= available_cores) {
      ncores <- available_cores
    }
    os_type <- Sys.info()["sysname"]
    if (os_type %in% c("Darwin", "Linux")) {
      return_value <- parallel::mclapply(
        X = X,
        FUN = FUN,
        mc.cores = ncores,
        ...
      )
    } else {
      cl <- parallel::makeCluster(spec = ncores)
      on.exit(
        expr = parallel::stopCluster(cl = cl)
      )
      return_value <- parallel::parLapply(
        cl = cl,
        X = X,
        fun = FUN,
        ...
      )
    }
  }
  return_value
}
