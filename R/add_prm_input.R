#'
#' @importFrom dplyr "%>%" bind_rows
#'
#' @export
#'
add_prm_input <- function(.input_tbl, ...){

  .input_tbl <- create_prm_input(...) %>%
    bind_rows(.input_tbl,.) %>%
    as_prm_tbl()

  return(.input_tbl)
}
