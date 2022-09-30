#'
#' @importFrom dplyr "%>%" full_join
#'
#' @export
#'
add_dssat_prm <- function(.prm_tbl, ...){

  .prm_tbl <- create_dssat_prm(...) %>%
    full_join(.prm_tbl,.) %>%
    as_dssat_prm_tbl()

  return(.prm_tbl)
}
