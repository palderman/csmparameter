#'
#' @importFrom dplyr  bind_rows
#'
#' @export
#'
prm_add_inp <- function(.input_tbl, ...){

  .input_tbl <- prm_create_inp_tbl(...) |>
    (\(.x) bind_rows(.input_tbl, .x))() |>
    as_prm_tbl()

  return(.input_tbl)
}
