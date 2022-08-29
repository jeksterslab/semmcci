LatexKnit <- function() {
  root <- rprojroot::is_rstudio_project
  detritus_folder <- root$find_file(
    "detritus"
  )
  Rtex_files <- list.files(
    root$find_file(
      "latex"
    ),
    pattern = "\\.Rtex$",
    full.names = TRUE,
    all.files = TRUE
  )
  if (length(Rtex_files) > 0) {
    lapply(
      X = Rtex_files,
      FUN = function(i) {
        tex_file <- sub(
          pattern = "\\.Rtex$",
          replacement = "\\.tex",
          x = basename(i)
        )
        knitr::knit(
          input = i,
          output = file.path(
            detritus_folder,
            tex_file
          )
        )
      }
    )
  }
}
