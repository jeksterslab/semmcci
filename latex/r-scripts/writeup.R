# find root directory
root <- rprojroot::is_rstudio_project
# source R_writeup
lapply(
  X = root$find_file(
    "r-writeup"
  ),
  FUN = function(x) {
    x <- list.files(
      path = x,
      pattern = "\\.R$",
      full.names = TRUE,
      all.files = TRUE,
      recursive = TRUE
    )
    if (length(x) > 0) {
      lapply(
        X = x,
        FUN = source
      )
    }
  }
)
