#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
path <- as.character(x = args[1])
source(
  file = file.path(
    path,
    ".setup",
    "scripts",
    "project.R"
  )
)
dot_library_folder <- file.path(
  path,
  ".library"
)
.libPaths(
  new = dot_library_folder
)
Sys.setenv(
  R_LIBS_USER = dot_library_folder
)
rProject::PkgDevel(
  path = path
)
rProject::PkgDevelGitHub(
  path = path
)
rProject::PkgProject(
  path = path,
  pkg = pkg_cran
)
if (length(x = pkg_github_ref) == 0) {
  rProject::PkgProjectGitHub(
    path = path,
    pkg = pkg_github
  )
} else {
  rProject::PkgProjectGitHub(
    path = path,
    pkg = pkg_github,
    ref = pkg_github_ref
  )
}
rProject::PkgProjectBioconductor(
  path = path,
  pkg = pkg_bioconductor
)
rProject::PkgProjectVersion(
  path = path,
  pkg = pkg_ver,
  ver = ver
)
warnings()
