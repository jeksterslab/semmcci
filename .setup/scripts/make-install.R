#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
path <- as.character(args[1])
if (
  file.exists(
    file.path(
      path,
      "DESCRIPTION"
    )
  )
) {
  devtools::document(path)
  devtools::install(path, dependencies = FALSE)
}
