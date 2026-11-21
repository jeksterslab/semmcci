#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
path <- as.character(x = args[1])
if (
  file.exists(
    file.path(
      path,
      "DESCRIPTION"
    )
  )
) {
  deps <- list.files(
    path = file.path(
      path,
      ".setup",
      "r-dependencies"
    ),
    pattern = ".*\\.R",
    full.names = TRUE, recursive = TRUE
  )
  if (length(x = deps) > 0) {
    # a helper renamed or deleted in functions must not stay in R/; a set
    # named after the package is left alone (the package's own files)
    sets <- setdiff(
      x = list.dirs(
        path = file.path(path, ".setup", "r-dependencies"),
        full.names = FALSE,
        recursive = FALSE
      ),
      y = read.dcf(file = file.path(path, "DESCRIPTION"), fields = "Package")
    )
    for (set in sets) {
      unlink(
        x = list.files(
          path = file.path(path, "R"),
          pattern = paste0("^", set, "-.*\\.R$"),
          full.names = TRUE
        )
      )
    }
    file.copy(
      from = deps,
      to = file.path(path, "R"),
      overwrite = TRUE
    )
  }
  cpp <- list.files(
    path = file.path(path, ".setup", "cpp"),
    pattern = "^.*\\.cpp",
    full.names = TRUE
  )
  if (length(x = cpp) > 0) {
    dir.create(
      path = file.path(path, "src"),
      showWarnings = FALSE,
      recursive = TRUE
    )
    unlink(
      x = file.path(
        path,
        "R",
        "RcppExports.R"
      )
    )
    unlink(
      x = file.path(
        path,
        "src",
        "RcppExports.cpp"
      )
    )
    namespace <- file.path(
      path,
      "NAMESPACE"
    )
    if (!file.exists(namespace)) {
      file.create(namespace)
    }
    cpp_file_fn <- file.path(
      path,
      "src",
      "source.cpp"
    )
    cpp_file <- file(
      description = cpp_file_fn,
      open = "w"
    )
    for (i in seq_along(along.with = cpp)) {
      writeLines(
        text = readLines(con = cpp[i]),
        con = cpp_file
      )
    }
    file.copy(
      from = file.path(path, ".setup", "cpp", "Makevars"),
      to = file.path(path, "src"),
      overwrite = TRUE
    )
    file.copy(
      from = file.path(path, ".setup", "cpp", "Makevars.win"),
      to = file.path(path, "src"),
      overwrite = TRUE
    )
    close(con = cpp_file)
    Rcpp::compileAttributes(pkgdir = path)
    roxygen2::roxygenize(
      package.dir = path,
      roclets = "rd"
    )
    unlink(x = namespace)
  }
  devtools::document(pkg = path)
  devtools::install(pkg = path, dependencies = FALSE, upgrade = FALSE)
}
warnings()
