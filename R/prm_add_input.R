#' Add input to model input data frame
#'
#' @export
#'
#' @param .input_df a data frame with model input information as created by
#'  \link{prm_create_input_df}
#'
#' @param ... parameters passed to \link{prm_create_input_df}
#'
prm_add_input <- function(.input_df, ...){

  .input_df <- prm_create_input_df(...) |>
    rbind(.input_df, y = _) |>
    as_prm_df()

  return(.input_df)
}
