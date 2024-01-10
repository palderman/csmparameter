#'
#' @importFrom dplyr  full_join
#'
#' @export
#'
prm_add <- function(.prm_tbl, ...){

  .prm_tbl <- prm_create(...) |>
    (\(.x) full_join(.prm_tbl, .x)
     )() |>
    as_prm_tbl()

  return(.prm_tbl)
}
