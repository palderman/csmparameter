#'
#' @export
#'
prm_add_expmt <- function(.expmt_df, ...){

  .expmt_tbl <- create_expmt(...) |>
    merge(.expmt_table, y = _, all = TRUE, by='filex') |>
    as_prm_expmt_df()

  return(.expmt_df)

}
