root <- rprojroot::is_rstudio_project
unlink(
  root$find_file(
    "semmcci.pdf"
  )
)
pack <- "semmcci"
path <- find.package(pack)
system(
  paste(
    shQuote(
      file.path(
        R.home("bin"),
        "R"
      )
    ),
    "CMD",
    "Rd2pdf",
    shQuote(
      path
    )
  )
)
