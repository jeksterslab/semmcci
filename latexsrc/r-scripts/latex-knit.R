LatexKnit <- function() {
  root <- rprojroot::is_rstudio_project
  dot_detritus_folder <- root$find_file(
    ".detritus"
  )
  rtex_files <- list.files(
    root$find_file(
      "latexsrc"
    ),
    pattern = "\\.Rtex$",
    full.names = TRUE,
    all.files = TRUE
  )
  if (length(rtex_files) > 0) {
    lapply(
      X = rtex_files,
      FUN = function(i) {
        tex_file <- sub(
          pattern = "\\.Rtex$",
          replacement = "\\.tex",
          x = basename(i)
        )
        knitr::knit(
          input = i,
          output = file.path(
            dot_detritus_folder,
            "tex",
            tex_file
          )
        )
      }
    )
  }
}
