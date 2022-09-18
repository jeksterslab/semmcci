LatexCompile <- function(clean = TRUE) {
  root <- rprojroot::is_rstudio_project
  tex_folder <- root$find_file(
    ".detritus",
    "tex"
  )
  pdf_folder <- root$find_file(
    "latexsrc",
    "pdf"
  )
  try(
    system(
      paste0(
        "latexmk -f -pdf -interaction=nonstopmode -output-directory=",
        pdf_folder,
        " ",
        paste0(
          tex_folder,
          "/*.tex"
        )
      )
    )
  )
  if (clean) {
    invisible(
      lapply(
        X = c(
          "\\.tex$",
          "\\.xml$",
          "\\.out$",
          "\\.log$",
          "\\.fls$",
          "\\.fdb_latexmk$",
          "\\.ent$",
          "\\.blg$",
          "\\.bcf$",
          "\\.bbl$",
          "\\.aux$"
        ),
        FUN = function(x) {
          invisible(
            lapply(
              X = list.files(
                pdf_folder,
                pattern = x,
                full.names = TRUE,
                all.files = TRUE
              ),
              FUN = unlink
            )
          )
        }
      )
    )
    unlink(
      paste0(
        tex_folder,
        "/tex-fig-*"
      ),
      recursive = TRUE
    )
  }
}
