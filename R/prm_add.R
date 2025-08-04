#'
#' @importFrom dplyr  full_join
#'
#' @export
#'
prm_add <- function(.prm_df, ...){

  .prm_df <- prm_create(...) |>
    (\(.x) full_join(.prm_df, .x)
     )() |>
    as_prm_df()

  return(.prm_df)
}
