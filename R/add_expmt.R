#'
#' @importFrom dplyr "%>%" full_join
#'
#' @export
#'
add_expmt <- function(.expmt_tbl, ...){

  .expmt_tbl <- create_expmt(...) %>%
    full_join(.expmt_table,.,by='filex') %>%
    as_expmt_tbl()

  return(.expmt_tbl)

}
