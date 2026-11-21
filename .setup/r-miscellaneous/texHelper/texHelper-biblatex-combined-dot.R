#' Create Biblatex File
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @returns Returns NULL.
#'
#' @param path Character string.
#'   Path to `*.bib` files.
#' @param output_path Character string.
#'   Output path.
#' @param fn_bib Character string.
#'   Biblatex filename.
#'
#' @family Bibliography Functions
#' @keywords texHelper biblatex internal
#' @noRd
.BiblatexCombined <- function(path,
                              output_path,
                              fn_bib = "bib.bib") {
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
      dir.create(
        path = output_path,
        showWarnings = FALSE
      )
      output_file <- file.path(
        output_path,
        fn_bib
      )
      con <- file(description = output_file)
      writeLines(
        text = bibs,
        con = con
      )
      close(con = con)
    } else {
      stop("length(bibs) == 0")
    }
  } else {
    stop(
      paste0(
        path,
        " does not exist."
      )
    )
  }
  invisible(x = NULL)
}
