#'
#' @importFrom dplyr  bind_rows
#'
#' @export
#'
add_prm_input <- function(.input_tbl, ...){

  .input_tbl <- create_prm_input(...) |>
    (\(.x) bind_rows(.input_tbl, .x))() |>
    as_prm_tbl()

  return(.input_tbl)
}
