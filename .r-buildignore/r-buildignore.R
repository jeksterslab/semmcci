root <- rprojroot::is_rstudio_project
default <- paste0(
  readLines(
    root$find_file(
      ".r-buildignore",
      "Rbuildignore"
    )
  ),
  collapse = "\n"
)
custom <- paste0(
  readLines(
    root$find_file(
      ".r-buildignore",
      "Rbuildignore-custom"
    )
  ),
  collapse = "\n"
)
con <- file(
  root$find_file(
    ".Rbuildignore"
  )
)
writeLines(
  text = paste0(
    default,
    "\n",
    custom,
    collapse = "\n"
  ),
  con = con
)
close(con)
