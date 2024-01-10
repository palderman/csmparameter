#'
#' @importFrom dplyr  full_join
#'
#' @export
#'
add_expmt <- function(.expmt_tbl, ...){

  .expmt_tbl <- create_expmt(...) |>
    (\(.x) full_join(.expmt_table, .x, by='filex'))() |>
    as_prm_expmt_tbl()

  return(.expmt_tbl)

}
