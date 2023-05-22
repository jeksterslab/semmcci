#' Standardized Solution from lavaan
#'
#' Computes the standardized solution for a `lavaan`
#' object with user supplied parameter vector.
#'
#' @details Standardized solution for the means is not yet supported.
#'
#' @author Shu Fai Cheung
#' @param est A vector of parameters.
#' @param object object of class `lavaan`.
#' @return A vector of the standardized solution, of the same
#'  length as the number of rows in the parameter table.
#'
#' @family Lavaan Helper Functions
#' @keywords lavHelper matrix standardized internal
#' @noRd
.StdLav <- function(est,
                    object) {
  lav_model <- object@Model

  # Set parameters

  lav_model_new <- lavaan::lav_model_set_parameters(
    lavmodel = lav_model,
    x = est
  )

  # Get the list of lavaan matrices with changed parameters
  # Adapted from lavaan::lav_object_inspect_modelmatrices
  # This block works regardless of the number of groups

  glist_new <- lav_model_new@GLIST
  for (i in seq_len(length(glist_new))) {
    dimnames(glist_new[[i]]) <- lav_model@dimNames[[i]]
  }
  ng <- lav_model@nblocks
  nmat <- lav_model@nmat
  gp_labels <- object@Data@group.label
  if (length(gp_labels) == 0) {
    gp_labels <- "single_group"
  }
  out <- vector(
    mode = "list",
    length = ng
  )
  isum <- 0
  for (i in seq_len(ng)) {
    out[[i]] <- glist_new[seq_len(nmat[i]) + isum]
    isum <- isum + nmat[i]
  }
  names(out) <- unlist(gp_labels)

  glist_new <- out

  # Do standardization by RAM matrices

  ram <- lapply(
    X = glist_new,
    FUN = .Lav2RAM
  )
  ram_std <- lapply(
    X = ram,
    FUN = .StdRAM
  )
  glist_std <- mapply(
    FUN = .RAM2Lav,
    ram_std,
    glist_new,
    MoreArgs = list(
      standardized = TRUE
    ),
    SIMPLIFY = FALSE
  )

  # Update GLIST

  if (ng == 1) {
    lav_model_new@GLIST <- glist_std[[1]]
  } else {
    lav_model_new@GLIST <- unlist(
      glist_std,
      recursive = FALSE
    )
  }

  # Get parameter values with type = "user" and extra = TRUE

  return(
    lavaan::lav_model_get_parameters(
      lavmodel = lav_model_new,
      type = "user",
      extra = TRUE
    )
  )
}
