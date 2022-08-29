# find root directory
root <- rprojroot::is_rstudio_project
# source r-load-all/r-load-all.R
source(
  root$find_file(
    "r-load-all",
    "r-load-all.R"
  )
)
# run tests_external
lapply(
  X = root$find_file("tests-external"),
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
