OSRelease <- function() {
  if (file.exists("/etc/os-release")) {
    os_release <- readLines("/etc/os-release")
  } else {
    os_release <- NULL
  }
  if (!is.null(os_release)) {
    vals <- as.list(sub('^.*="?(.*?)"?$', "\\1", os_release))
    names(vals) <- sub("^(.*)=.*$", "\\1", os_release)
    out <- list(
      id = vals[["ID"]],
      version = vals[["VERSION_ID"]]
    )
    if ("VERSION_CODENAME" %in% names(vals)) {
      out$codename <- vals[["VERSION_CODENAME"]]
    } else {
      out$codename <- vals[["PRETTY_NAME"]]
    }
    out <- as.vector(out)
    names(out) <- c(
      "distro",
      "release",
      "codename"
    )
    return(out)
  } else {
    return(NULL)
  }
}
