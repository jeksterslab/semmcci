################################################################################
# ====[ rprojroot ]=============================================================
################################################################################
## set library -----------------------------------------------------------------
library(
  "rprojroot",
  lib = ".library"
)
root <- rprojroot::is_rstudio_project
dot_library_folder <- root$find_file(
  ".library"
)
dir.create(
  ".library",
  showWarnings = FALSE,
  recursive = TRUE
)
Sys.setenv(
  R_LIBS = dot_library_folder
)
Sys.setenv(
  R_LIBS_USER = dot_library_folder
)
.libPaths(
  dot_library_folder,
  include.site = FALSE
)
## process ---------------------------------------------------------------------
installed <- installed.packages(lib = dot_library_folder)
pkg_installed <- installed[, "Package"]
if (!("remotes" %in% pkg_installed)) {
  install.packages(
    "remotes",
    repos = "https://cran.rstudio.com/",
    lib = ".library",
    dependencies = TRUE
  )
}
