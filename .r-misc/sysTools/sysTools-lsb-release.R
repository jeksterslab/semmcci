LSBRelease <- function() {
  if (
    nzchar(
      Sys.which("lsb_release")
    )
  ) {
    # distributions with lsb_release
    out <- sapply(
      X = c(
        "-is",
        "-rs",
        "-cs"
      ),
      FUN = function(arg) {
        system(
          paste(
            "lsb_release",
            arg
          ),
          intern = TRUE
        )
      }
    )
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
