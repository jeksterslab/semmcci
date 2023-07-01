#' CRAN Check using rhub
CRANCheck <- function() {
  root <- rprojroot::is_rstudio_project
  path <- dirname(
    root$find_file(
      "project.Rproj"
    )
  )
  source(
    file.path(
      path,
      "tools",
      "project.R"
    )
  )
  rhub::check_for_cran(
    path = path,
    email = r_email
  )
}
