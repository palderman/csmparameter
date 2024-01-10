#' @importFrom dplyr  group_by mutate group_modify
#' @importFrom DSSAT run_dssat
#'
add_output_tbl <- function(.expmt){

  write_dssbatch(x=.expmt$filex_name,trtno=.expmt$trno[[1]])

  run_dssat(suppress_output = TRUE)

  .expmt <- .expmt |>
    group_by(filex_name) |>
    group_modify(~mutate(.x,out_tbl = list(find_output_variables(.x))))

  return(.expmt)

}
