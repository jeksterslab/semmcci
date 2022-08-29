################################################################################
# ====[ rprojroot ]=============================================================
################################################################################
## set library -----------------------------------------------------------------
dot_library_folder <- ".library"
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
installed <- installed.packages(lib = ".library")
pkg_installed <- installed[, "Package"]
if (!("rprojroot" %in% pkg_installed)) {
  install.packages(
    "rprojroot",
    repos = "https://cran.rstudio.com/",
    lib = ".library",
    dependencies = TRUE
  )
}
