#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
path <- as.character(x = args[1])
source(
  file = file.path(
    path,
    ".setup",
    "scripts",
    "project.R"
  )
)
# Secrets are read from the local machine, never from the project.
# The first file that defines `tokens` wins:
#   1. Sys.getenv("RPROJECT_SECRETS")
#   2. ~/.config/rproject/<project>.R
#   3. ~/.config/rproject/secrets.R
#   4. .setup/scripts/secrets.R (legacy; prints a warning)
secrets_legacy <- file.path(
  path,
  ".setup",
  "scripts",
  "secrets.R"
)
secrets_candidates <- c(
  Sys.getenv(x = "RPROJECT_SECRETS"),
  file.path(
    "~",
    ".config",
    "rproject",
    paste0(project, ".R")
  ),
  file.path(
    "~",
    ".config",
    "rproject",
    "secrets.R"
  ),
  secrets_legacy
)
secrets_candidates <- secrets_candidates[
  nzchar(x = secrets_candidates) & file.exists(secrets_candidates)
]
tokens <- NULL
secrets_path <- NULL
for (secrets_candidate in secrets_candidates) {
  secrets_env <- new.env(parent = globalenv())
  sys.source(
    file = secrets_candidate,
    envir = secrets_env
  )
  tokens <- get0(
    x = "tokens",
    envir = secrets_env,
    inherits = FALSE
  )
  if (length(x = tokens) > 0) {
    secrets_path <- secrets_candidate
    break
  }
}
rm(list = ls(pattern = "^secrets_(env|candidate|candidates)$"))
if (!is.null(x = secrets_path)) {
  if (secrets_path == secrets_legacy) {
    warning(
      paste0(
        "\n\nSecrets were read from ",
        secrets_legacy,
        ".\nMove them to ~/.config/rproject/secrets.R (chmod 600)",
        " and remove the tokens from Overleaf.\n\n"
      ),
      call. = FALSE
    )
  } else {
    secrets_mode <- file.info(secrets_path)$mode
    if (
      bitwAnd(
        a = as.integer(x = secrets_mode),
        b = strtoi(x = "077", base = 8L)
      ) != 0L
    ) {
      Sys.chmod(
        paths = secrets_path,
        mode = "600"
      )
      message(
        paste0(
          "\n\nPermissions of ",
          secrets_path,
          " changed from ",
          format(x = secrets_mode),
          " to 600.\n\n"
        )
      )
    }
  }
  message(
    paste0(
      "\n\nSecrets read from ",
      secrets_path,
      ": ",
      paste(names(x = tokens), collapse = ", "),
      "\n\n"
    )
  )
  keys <- names(x = tokens)
  env <- rep(x = "", times = length(x = keys))
  for (i in seq_along(along.with = tokens)) {
    env[i] <- paste0(
      "export",
      " ",
      keys[i],
      "=",
      tokens[i]
    )
  }
  writeLines(text = env, con = "~/.bash-secrets")
  Sys.setenv(
    GITHUB_PAT = tokens["GITHUB_PAT"]
  )
} else {
  message(
    paste0(
      "\n\n",
      "No secrets found.",
      " Put tokens in ~/.config/rproject/secrets.R",
      " (not needed in GitHub Actions).",
      "\n\n"
    )
  )
  tokens <- NULL
}
dot_library_folder <- file.path(
  path,
  ".library"
)
dir.create(
  path = dot_library_folder,
  showWarnings = FALSE,
  recursive = TRUE
)
dot_gitignore_file <- file.path(
  dot_library_folder,
  ".gitignore"
)
if (!file.exists(dot_gitignore_file)) {
  dot_gitignore <- c(
    "*",
    "*/"
  )
  con <- file(description = dot_gitignore_file)
  writeLines(
    text = dot_gitignore,
    con = con,
    sep = "\n"
  )
  close(con = con)
}
.libPaths(
  new = dot_library_folder
)
Sys.setenv(
  R_LIBS_USER = dot_library_folder
)
installed <- utils::installed.packages()
pkg_installed <- installed[, "Package"]
if (!("pak" %in% pkg_installed)) {
  utils::install.packages(
    pkgs = "pak",
    repos = c(REPO_NAME = "https://packagemanager.posit.co/cran/latest"),
    lib = dot_library_folder,
    quiet = TRUE
  )
}
# install rProject alone into .library: pak looks only in `lib` for what is
# installed, so with dependencies it would rebuild from source every package
# the image already has (spec 2026-10-13-make-project-pak); the dependencies
# come from the image, which .libPaths() keeps after .library, locally and in
# GitHub Actions (rProject 0.0.57: make cleanpkg removes .library only)
rproject_install <- function(ref) {
  message(
    paste0(
      "\n\nInstalling rProject ",
      ref,
      " into .library (dependencies from the image).\n\n"
    )
  )
  rproject_missing <- paste0(
    "\n\nIf a dependency is missing, add it to the image",
    " (docker-docs or the project's .sif);",
    " make project does not install dependencies into .library.\n\n"
  )
  tryCatch(
    expr = pak::pkg_install(
      pkg = ref,
      lib = dot_library_folder,
      dependencies = FALSE,
      upgrade = FALSE
    ),
    error = function(e) {
      stop(
        paste0(
          "\n\nrProject could not be installed in .library:\n",
          conditionMessage(c = e),
          rproject_missing
        ),
        call. = FALSE
      )
    }
  )
  # load it in a fresh R process; a missing dependency belongs in the image
  rproject_load <- suppressWarnings(
    expr = system2(
      command = file.path(R.home(component = "bin"), "Rscript"),
      args = c(
        "--vanilla",
        "-e",
        shQuote(
          string = paste0(
            ".libPaths(c(\"",
            dot_library_folder,
            "\", .libPaths())); loadNamespace(\"rProject\")"
          )
        )
      ),
      stdout = TRUE,
      stderr = TRUE
    )
  )
  if (!is.null(x = attr(x = rproject_load, which = "status"))) {
    stop(
      paste0(
        "\n\nrProject was installed in .library but does not load:\n",
        paste(rproject_load, collapse = "\n"),
        rproject_missing
      ),
      call. = FALSE
    )
  }
}
# the rProject to install (rProject 0.0.57: lock by default): the one in
# .setup/scripts/rproject.lock (written by make auto), locally and in GitHub
# Actions, so that a rebuild uses the rProject that built the release;
# make upgrade (or make project LOCK=FALSE) and a project without a lock
# follow the current commit on GitHub
rproject_mode <- toupper(x = c(args[2], "")[1])
# docker-* follow the latest rProject at every build (no lock)
# the project's type (project.R)
rproject_type <- get0(
  x = "type",
  envir = globalenv(),
  inherits = FALSE,
  ifnotfound = ""
)
rproject_upgrade <- rproject_mode %in% c("UPGRADE", "FALSE") ||
  identical(x = rproject_type, y = "docker")
rproject_lock <- file.path(path, ".setup", "scripts", "rproject.lock")
rproject_lock_env <- new.env()
if (file.exists(rproject_lock)) {
  sys.source(file = rproject_lock, envir = rproject_lock_env)
}
rproject_locked <- get0(
  x = "rproject_sha",
  envir = rproject_lock_env,
  inherits = FALSE,
  ifnotfound = NA_character_
)
if (rproject_upgrade) {
  rproject_locked <- NA_character_
  message(
    paste0(
      "\n\nmake upgrade: rProject follows GitHub; the next make auto",
      " records it and the running image in the lock.\n\n"
    )
  )
} else if (!is.na(x = rproject_locked)) {
  message(paste0("\n\nrProject from the lock: ", rproject_locked, "\n\n"))
}
rproject_head <- suppressWarnings(
  expr = system2(
    command = "git",
    args = c(
      "ls-remote",
      "https://github.com/ijapesigan/rProject.git",
      "HEAD"
    ),
    stdout = TRUE,
    stderr = FALSE,
    env = c(
      "GIT_CONFIG_GLOBAL=/dev/null",
      "GIT_TERMINAL_PROMPT=0"
    )
  )
)
rproject_head <- substr(
  x = c(rproject_head, NA_character_)[1],
  start = 1,
  stop = 40
)
rproject_sha <- suppressWarnings(
  expr = utils::packageDescription(
    pkg = "rProject",
    lib.loc = dot_library_folder,
    fields = "RemoteSha"
  )
)
# the image must provide rProject's Imports at their minimum versions;
# make project never installs them into .library
rproject_imports <- function(ref) {
  description <- tempfile()
  ok <- suppressWarnings(
    expr = tryCatch(
      expr = utils::download.file(
        url = paste0(
          "https://raw.githubusercontent.com/ijapesigan/rProject/",
          ref,
          "/DESCRIPTION"
        ),
        destfile = description,
        quiet = TRUE
      ) == 0,
      error = function(e) {
        FALSE
      }
    )
  )
  if (isTRUE(x = ok)) {
    imports <- read.dcf(file = description, fields = "Imports")[1, 1]
    imports <- trimws(
      x = strsplit(
        x = gsub(pattern = "\n", replacement = " ", x = imports),
        split = ","
      )[[1]]
    )
    imports <- imports[nzchar(x = imports)]
    name <- sub(pattern = "\\s*\\(.*$", replacement = "", x = imports)
    minimum <- ifelse(
      test = grepl(pattern = ">=", x = imports),
      yes = sub(
        pattern = "^.*>=\\s*([^) ]+).*$",
        replacement = "\\1",
        x = imports
      ),
      no = NA_character_
    )
    have <- vapply(
      X = name,
      FUN = function(x) {
        out <- suppressWarnings(
          expr = utils::packageDescription(pkg = x, fields = "Version")
        )
        ifelse(
          test = is.null(x = out) || is.na(x = out),
          yes = NA_character_,
          no = out
        )
      },
      FUN.VALUE = character(length = 1)
    )
    bad <- is.na(x = have) | (
      !is.na(x = minimum) &
        !is.na(x = have) &
        mapply(
          FUN = function(a, b) {
            utils::compareVersion(a = a, b = b) < 0
          },
          a = ifelse(test = is.na(x = have), yes = "0", no = have),
          b = ifelse(test = is.na(x = minimum), yes = "0", no = minimum)
        )
    )
    if (any(bad)) {
      stop(
        paste0(
          "\n\nThe image does not provide what rProject ",
          substr(x = ref, start = 1, stop = 7),
          " imports: ",
          paste0(
            name[bad],
            ifelse(
              test = is.na(x = minimum[bad]),
              yes = "",
              no = paste0(" (>= ", minimum[bad], ")")
            ),
            collapse = ", "
          ),
          ".\nRebuild docs.sif (build docs), or use the image that",
          " built the release.\n\n"
        ),
        call. = FALSE
      )
    }
  }
  unlink(x = description)
}
rproject_target <- ifelse(
  test = is.na(x = rproject_locked),
  yes = rproject_head,
  no = rproject_locked
)
# rProject builds itself from source: install the working copy, so that make
# runs this rProject (its GitHub HEAD is the last public release, which may
# not read the current project files)
rproject_self <- identical(x = rproject_type, y = "rProject")
if (rproject_self) {
  message("\n\nInstalling rProject from this folder into .library.\n\n")
  # in a separate R process, so that this one loads the installed rProject
  # (not a namespace left by roxygen); GitHub Actions only installs, with
  # NAMESPACE and man/ as published (make all wrote them)
  rproject_self_code <- paste0(
    ".libPaths(c(\"", dot_library_folder, "\", .libPaths())); ",
    if (identical(x = Sys.getenv(x = "GITHUB_ACTIONS"), y = "true")) {
      ""
    } else {
      paste0("devtools::document(pkg = \"", path, "\"); ")
    },
    "install.packages(\"", path, "\", repos = NULL, type = \"source\", ",
    "lib = \"", dot_library_folder, "\")"
  )
  rproject_self_status <- system2(
    command = file.path(R.home(component = "bin"), "Rscript"),
    args = c("-e", shQuote(string = rproject_self_code))
  )
  if (!identical(x = as.integer(x = rproject_self_status), y = 0L)) {
    stop(
      "\n\nrProject could not be installed from this folder.\n\n",
      call. = FALSE
    )
  }
} else if (is.na(x = rproject_target)) {
  if (is.na(x = rproject_sha)) {
    rproject_install(ref = "ijapesigan/rProject")
  } else {
    message(
      "\n\nCould not reach GitHub; keeping the installed rProject.\n\n"
    )
  }
} else if (!identical(x = rproject_sha, y = rproject_target)) {
  rproject_imports(ref = rproject_target)
  rproject_install(
    ref = paste0(
      "ijapesigan/rProject",
      "@",
      rproject_target
    )
  )
}
if (
  !rproject_self &&
    !is.na(x = rproject_locked) &&
    !is.na(x = rproject_head) &&
    !identical(x = rproject_locked, y = rproject_head)
) {
  message(
    paste0(
      "\n\nA newer rProject is on GitHub (",
      substr(x = rproject_head, start = 1, stop = 7),
      "); the lock keeps ",
      substr(x = rproject_locked, start = 1, stop = 7),
      ". make upgrade moves the project forward.\n\n"
    )
  )
}
# a local build in another image than the lock's: say so (an older sif is
# the user's to keep; the next make auto records the running one)
rproject_image <- get0(
  x = "image",
  envir = rproject_lock_env,
  inherits = FALSE,
  ifnotfound = NA_character_
)
if (
  !is.na(x = rproject_image) &&
    grepl(pattern = "@", x = rproject_image) &&
    file.exists("/.singularity.d/labels.json")
) {
  rproject_running <- grep(
    pattern = "org.opencontainers.image.base.digest",
    x = readLines(con = "/.singularity.d/labels.json", warn = FALSE),
    fixed = TRUE,
    value = TRUE
  )[1]
  if (
    !is.na(x = rproject_running) &&
      !grepl(
        pattern = sub(pattern = "^.*@", replacement = "", x = rproject_image),
        x = rproject_running,
        fixed = TRUE
      )
  ) {
    message(
      paste0(
        "\n\nThis sif is not the image in the lock (",
        rproject_image,
        "). The next make auto records the running image.\n\n"
      )
    )
  }
}
rProject::Project(
  path = path
)
rProject::Profile(
  path = path,
  project = project
)
rProject::Environment(
  path = path,
  project = project,
  tokens = tokens
)
rProject::BuildIgnore(
  path = path,
  project = project,
  add = ignore
)
rProject::Binary(
  path = path
)
rProject::License(
  path = path,
  type = license
)
# template files of the project's type (rProject 0.0.53 or later)
if (exists(x = "Template", envir = asNamespace(ns = "rProject"))) {
  rProject::Template(
    path = path
  )
}
# per-project AI instructions (rProject 0.0.34 or later)
if (exists(x = "AI", envir = asNamespace(ns = "rProject"))) {
  rProject::AI(
    path = path
  )
}
warnings()
