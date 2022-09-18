LatexMake <- function(clean = TRUE) {
  root <- rprojroot::is_rstudio_project
  source(
    root$find_file(
      "latexsrc",
      "r-scripts",
      "latex-knit.R"
    )
  )
  source(
    root$find_file(
      "latexsrc",
      "r-scripts",
      "latex-compile.R"
    )
  )
  try(
    LatexKnit()
  )
  try(
    LatexCompile(clean = clean)
  )
}
