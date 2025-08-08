#' Add a parameter to a parameter estimation data frame
#'
#' @export
#'
#' @param .prm_df a data frame that contains parameter information as generated
#'  by \link{prm_create_prm_df}
#'
#' @param ... arguments passed to \link{prm_create_prm_df} (see
#'  \link{prm_create_prm_df} for details)
#'
prm_add_prm <- function(.prm_df, ...){

  .prm_df <- prm_create_prm_df(...) |>
    merge(.prm_df, y = _, all = TRUE) |>
    as_prm_df()

  return(.prm_df)
}
