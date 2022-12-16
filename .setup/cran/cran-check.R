#' CRAN Check using rhub
CRANCheck <- function(path) {
  cat(
    paste0(
      "Make sure that the email address is validated.\n",
      "Consider running:\n",
      "rhub::validate_email(email = \"r.jeksterslab@gmail.com\")"
    )
  )
  rhub::check_for_cran(
    path = path,
    email = "r.jeksterslab@gmail.com"
  )
}
