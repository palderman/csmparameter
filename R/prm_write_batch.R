#'
#' @importFrom dplyr  select
#' @importFrom tidyr unnest
#' @importFrom DSSAT write_dssbatch
#'
#' @export
#'
prm_write_batch.expmt_df <- function(expmt_df){

  expmt_df |>
    select(filex_name,trno) |>
    unnest(trno) |>
    (\(.x) write_dssbatch(x = .x$filex_name, trtno = .x$trno)
     )()

}
