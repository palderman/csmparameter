#'
#' @importFrom dplyr  select
#' @importFrom tidyr unnest
#' @importFrom DSSAT write_dssbatch
#'
#' @export
#'
write_dssbatch.expmt_tbl <- function(expmt_tbl){

  expmt_tbl |>
    select(filex_name,trno) |>
    unnest(trno) |>
    (\(.x) write_dssbatch(x = .x$filex_name, trtno = .x$trno)
     )()

}
