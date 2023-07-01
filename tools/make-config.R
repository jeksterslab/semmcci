#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
path <- as.character(args[1])
source(
  file.path(
    path,
    "tools",
    "project.R"
  )
)
rProject::ConfigFiles(
  git_user = git_user
)
