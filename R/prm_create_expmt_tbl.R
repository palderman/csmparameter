#' @export
#'
#' @importFrom tibble tibble
#' @importFrom dplyr  group_by group_map bind_rows
#' @importFrom purrr map
#'
prm_create_expmt_tbl <- function(filex_name, trno = NULL, data_types = NULL,
                                 rewrite_filex = FALSE){
  if(is.null(trno)){
    filex_trno_tbl <- tibble(filex_name = filex_name,
                             trno = map(filex_name,~{NULL}),
                             data_types = data_types)
  }else{
    filex_trno_tbl <- tibble(filex_name = filex_name,
                             trno = trno,
                             data_types = data_types)
  }
  expmt_tbl <- filex_trno_tbl |>
    group_by(filex_name) |>
    group_map(~create_expmt(filex_name = .y$filex_name,
                            trno = .x$trno[[1]],
                            data_types = .x$data_types[[1]],
                            rewrite_filex = rewrite_filex)) |>
    bind_rows()
  return(expmt_tbl)
}
