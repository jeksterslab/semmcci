cat("Make sure to validate email first before proceeding.\n")
# find root directory
root <- rprojroot::is_rstudio_project
# root_folder
root_folder <- dirname(
  root$find_file(
    "project.Rproj"
  )
)
rhub::check_for_cran(
  path = root_folder,
  email = "r.jeksterslab@gmail.com"
)
