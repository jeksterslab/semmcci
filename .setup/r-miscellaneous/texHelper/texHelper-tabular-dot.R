#' Create Tabular Content from a Data Frame or A Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @returns Returns a character string.
#'
#' @param x Data frame of matrix.
#' @param digits Digits for numeric values.
#' @param open Character string.
#'   Opening delimiter around numeric values.
#' @param close Character string.
#'   Closing delimiter around numeric values.
#' @param init Character string or `NULL`.
#'   Text placed before every row, if not `NULL`.
#'
#' @family Tabular Functions
#' @keywords texHelper tabular internal
#' @noRd
.Tabular <- function(x,
                     digits = 2,
                     open = "$",
                     close = "$",
                     init = NULL) {
  stopifnot(
    is.matrix(x = x) || is.data.frame(x = x)
  )
  dims <- dim(x = x)
  y <- matrix(
    data = NA,
    nrow = dims[1],
    ncol = dims[2]
  )
  for (j in seq_len(length.out = dims[2])) {
    for (i in seq_len(length.out = dims[1])) {
      if (is.numeric(x = x[i, j])) {
        y[i, j] <- paste0(
          open,
          format(
            x = round(
              x = x[i, j],
              digits = digits
            ),
            nsmall = digits
          ),
          close
        )
      } else {
        y[i, j] <- x[i, j]
      }
    }
  }
  z <- rep(x = NA, times = dims[1])
  for (i in seq_len(length.out = dims[1])) {
    z[i] <- paste(
      y[i, ],
      collapse = " & "
    )
  }
  if (is.null(x = init)) {
    z <- paste(
      z,
      collapse = " \\\\"
    )
  } else {
    z <- paste(
      init,
      " & ",
      z,
      collapse = " \\\\"
    )
  }
  paste0(
    z,
    " \\\\"
  )
}
