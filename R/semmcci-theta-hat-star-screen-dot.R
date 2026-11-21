#' Sampling Distribution of Parameter Estimates with Screening
#'
#' Generates Monte Carlo draws with [.ThetaHatStar()],
#' screens every draw at once with `screen`,
#' and redraws each inadmissible draw, one at a time and in row order,
#' until it passes.
#' The random number stream is the same as drawing all rows first
#' and then redrawing the inadmissible ones in order.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams .ThetaHatStar
#' @param screen Function.
#'   Takes a numeric matrix of draws (rows)
#'   and returns a logical vector,
#'   `TRUE` for admissible draws.
#' @param append Numeric vector or `NULL`.
#'   Values appended to every draw before screening,
#'   for example, fixed parameters.
#' @param iter Positive integer.
#'   Maximum number of redraws for one draw.
#'   A draw that fails `iter` times is dropped.
#' @return Returns a numeric matrix of admissible draws.
#'
#' @family Monte Carlo in Structural Equation Modeling Functions
#' @keywords semmcci parameters internal
#' @noRd
.ThetaHatStarScreen <- function(R = 20000L,
                                scale,
                                location,
                                screen,
                                append = NULL,
                                decomposition = "eigen",
                                pd = TRUE,
                                tol = 1e-06,
                                iter = 1000L) {
  draws <- .ThetaHatStar(
    R = R,
    scale = scale,
    location = location,
    decomposition = decomposition,
    pd = pd,
    tol = tol
  )$thetahatstar
  if (!is.null(x = append)) {
    draws <- cbind(
      draws,
      matrix(
        data = append,
        nrow = R,
        ncol = length(x = append),
        byrow = TRUE
      )
    )
  }
  ok <- screen(draws)
  for (i in which(x = !ok)) {
    count <- 0L
    repeat {
      x <- .ThetaHatStar(
        R = 1L,
        scale = scale,
        location = location,
        decomposition = decomposition,
        pd = FALSE
      )$thetahatstar
      if (!is.null(x = append)) {
        x <- cbind(
          x,
          matrix(
            data = append,
            nrow = 1L
          )
        )
      }
      if (screen(x)) {
        draws[i, ] <- x
        ok[i] <- TRUE
        break
      }
      count <- count + 1L
      if (count >= iter) {
        break
      }
    }
  }
  draws[ok, , drop = FALSE]
}
