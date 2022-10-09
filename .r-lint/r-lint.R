# find root directory
root <- rprojroot::is_rstudio_project
# root_folder
root_folder <- dirname(
  root$find_file(
    "project.Rproj"
  )
)
dirs <- list.dirs(path = root_folder, full.names = FALSE, recursive = FALSE)
styles <- c(
  "CamelCase",
  "snake_case",
  "symbols"
)
filetype <- c(
  ".R",
  ".Rmd"
)

cat("\nlatexsrc/r-scripts\n")
latexsrc <- root$find_file(
  "latexsrc",
  "r-scripts"
)
styler::style_dir(
  latexsrc,
  filetype = filetype
)
lintr::lint_dir(
  latexsrc,
  lintr::object_name_linter(
    styles = styles
  )
)

cat("\n.r-*\n")
dot_r <- dirs[grep(pattern = "^\\.r-.*$", x = dirs)]
lapply(
  X = dot_r,
  FUN = function(path) {
    styler::style_dir(
      file.path(root_folder, path),
      filetype = filetype
    )
    lintr::lint_dir(
      file.path(root_folder, path),
      lintr::object_name_linter(
        styles = styles
      )
    )
  }
)

cat("\n.tests-*\n")
dot_tests <- dirs[grep(pattern = "^\\.tests-.*$", x = dirs)]
lapply(
  X = dot_tests,
  FUN = function(path) {
    styler::style_dir(
      file.path(root_folder, path),
      filetype = filetype
    )
    lintr::lint_dir(
      file.path(root_folder, path),
      lintr::object_name_linter(
        styles = styles
      )
    )
  }
)

cat("\nvignettes\n")
styler::style_dir(
  file.path(root_folder, "vignettes"),
  filetype = filetype
)
lintr::lint_dir(
  file.path(root_folder, "vignettes"),
  lintr::object_name_linter(
    styles = styles
  )
)

cat("\ntests\n")
styler::style_dir(
  file.path(root_folder, "tests"),
  filetype = filetype
)
lintr::lint_dir(
  file.path(root_folder, "tests"),
  lintr::object_name_linter(
    styles = styles
  )
)

cat("\nR\n")
styler::style_dir(
  file.path(root_folder, "R"),
  filetype = filetype
)
lintr::lint_dir(
  file.path(root_folder, "R"),
  lintr::object_name_linter(
    styles = styles
  )
)
