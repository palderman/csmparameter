#'
#' @importFrom dplyr  bind_rows
#'
#' @export
#'
prm_add_inp <- function(.input_df, ...){

  .input_df <- prm_create_inp_df(...) |>
    (\(.x) bind_rows(.input_df, .x))() |>
    as_prm_df()

  return(.input_df)
}
