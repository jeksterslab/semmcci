#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
################################################################################
# ====[ library ]===============================================================
################################################################################
dot_library_folder <- file.path(
  as.character(args[1]),
  ".library"
)
dir.create(
  dot_library_folder,
  showWarnings = FALSE,
  recursive = TRUE
)
.libPaths(
  dot_library_folder
)
Sys.setenv(
  R_LIBS_USER = dot_library_folder
)
################################################################################
# ====[ rprojroot ]=============================================================
################################################################################
installed <- installed.packages()
pkg_installed <- installed[, "Package"]
if (!("rprojroot" %in% pkg_installed)) {
  install.packages(
    "rprojroot",
    repos = c(REPO_NAME = "https://packagemanager.rstudio.com/all/latest"),
    lib = dot_library_folder
  )
}

# find root directory
root <- rprojroot::is_rstudio_project

# source functions
sys_tools <- list.files(
  path = root$find_file(
    ".r-misc",
    "sysTools"
  ),
  full.names = TRUE
)
for (i in seq_along(sys_tools)) {
  source(sys_tools[i])
}

# system
os <- OS()
if (os == "osx") {
  system(
    paste(
      "bash",
      root$find_file(
        ".sys",
        "sys-osx.sh"
      )
    )
  )
}
if (os == "windows") {
  system(
    paste(
      "bash",
      root$find_file(
        ".sys",
        "sys-windows.sh"
      )
    )
  )
}
if (os == "linux") {
  distro <- Distro()
  if (is.null(distro)) {
    stop("Unknown Linux distribution.")
  } else {
    distro_name <- tolower(distro["distro"])
    if (distro_name == "ubuntu") {
      system(
        paste(
          "bash",
          root$find_file(
            ".sys",
            "sys-ubuntu.sh"
          )
        )
      )
    }
    if (distro_name == "arch") {
      system(
        paste(
          "bash",
          root$find_file(
            ".sys",
            "sys-archlinux.sh"
          )
        )
      )
    }
  }
}
