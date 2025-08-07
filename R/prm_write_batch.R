#'
#' @export
#'
prm_write_batch.expmt_df <- function(expmt_df){

  expmt_df |>
    subset(select = c("filex_name", "trno")) |>
    with({
      setNames(trno, filex_name) |>
        stack() |>
        setNames(c("trno", "filex_name"))
    }) |>
    with(DSSAT::write_dssbatch(x = filex_name, trtno = trno))

}
