#' Create Array from a Data Frame or A Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @returns Returns a character string.
#'
#' @param x Data frame of matrix.
#' @param digits Digits for numeric values.
#'
#' @family Array Functions
#' @keywords texHelper array internal
#' @noRd
.Array <- function(x,
                   digits = 2) {
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
          format(
            x = round(
              x = x[i, j],
              digits = digits
            ),
            nsmall = digits
          )
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
  z <- paste(
    z,
    collapse = " \\\\"
  )
  z <- paste0(
    z,
    " \\\\"
  )
  paste(
    "\\begin{array}",
    "{",
    paste(
      rep(
        x = "c",
        times = dims[2]
      ),
      collapse = ""
    ),
    "}",
    "\n",
    z,
    "\n",
    "\\end{array}",
    sep = "",
    collapse = ""
  )
}
