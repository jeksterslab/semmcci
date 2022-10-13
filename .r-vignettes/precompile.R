root <- rprojroot::is_rstudio_project
root_folder <- dirname(
  root$find_file(
    "project.Rproj"
  )
)
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
  file.copy(
    from = file.path(vignettes_source, orig),
    to = vignettes_folder,
    overwrite = TRUE
  )
  target <- gsub(
    pattern = "\\.orig$",
    replacement = "",
    x = orig
  )
  for (i in seq_along(orig)) {
    knitr::knit(
      file.path(vignettes_folder, orig[i]),
      file.path(vignettes_folder, target[i])
    )
  }
}
figs <- list.files(
  path = root_folder,
  pattern = "^fig-vignettes-.",
  full.names = FALSE,
  all.files = TRUE,
  recursive = FALSE
)
if (length(figs) > 0) {
  file.copy(
    from = figs,
    to = vignettes_folder,
    overwrite = TRUE
  )
  unlink(figs)
}
