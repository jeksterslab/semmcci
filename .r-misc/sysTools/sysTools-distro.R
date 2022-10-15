Distro <- function() {
  lsb <- LSBRelease()
  if (is.null(lsb)) {
    os_release <- OSRelease()
    if (is.null(os_release)) {
      stop(
        "`LSBRelease` and `OSRelease` failed."
      )
    } else {
      return(os_release)
    }
  } else {
    return(lsb)
  }
}
