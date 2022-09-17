root <- rprojroot::is_rstudio_project
vignettes_folder <- root$find_file(
  "vignettes"
)
vignettes_source <- root$find_file(
  ".r-vignettes"
)
orig <- list.files(
  path = vignettes_source,
  pattern = "\\.orig$",
  full.names = FALSE,
  all.files = TRUE,
  recursive = FALSE
)
if (length(orig) > 0) {
  target <- gsub(
    pattern = "\\.orig$",
    replacement = "",
    x = orig
  )
  for (i in seq_along(orig)) {
    knitr::knit(
      file.path(vignettes_source, orig[i]),
      file.path(vignettes_folder, target[i])
    )
  }
}
