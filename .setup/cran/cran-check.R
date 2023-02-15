#' CRAN Check using rhub
CRANCheck <- function(path = NULL) {
  if (is.null(path)) {
    path <- getwd()
  }
  rhub::check_for_cran(
    path = path,
    email = "r.jeksterslab@gmail.com"
  )
}
