#'
#' @importFrom dplyr "%>%" bind_rows
#'
#' @export
#'
add_dssat_input <- function(.input_tbl,...){

  .input_tbl <- create_dssat_input(...) %>%
    bind_rows(.input_tbl,.) %>%
    as_dssat_prm_tbl()

  return(.input_tbl)
}
