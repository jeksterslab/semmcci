#' Create a Single Biblatex .bib File
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @returns Writes a text file.
#'
#' @param path Character string.
#'   Path to `*.bib` files.
#' @param output_path Character string.
#'   Output path.
#'
#' @family Bibliography Functions
#' @keywords texHelper biblatex internal
#' @noRd
.CreateBib <- function(path,
                       output_path) {
  if (dir.exists(paths = path)) {
    bibs <- list.files(
      path = path,
      pattern = "\\.bib$",
      full.names = TRUE,
      all.files = TRUE
    )
    if (length(x = bibs) > 0) {
      bibs <- unlist(
        x = lapply(
          X = bibs,
          FUN = readLines
        )
      )
      con <- file(
        description = file.path(
          output_path,
          "bib.bib"
        )
      )
      writeLines(
        text = bibs,
        con = con
      )
      close(con = con)
    }
  }
}
