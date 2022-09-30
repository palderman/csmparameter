#' @export
#'
#' @importFrom tibble tibble
#' @importFrom dplyr "%>%" group_by group_map bind_rows
#' @importFrom purrr map
#'
create_expmt_tbl <- function(filex_name, trno=NULL, data_types=NULL,
                             rewrite_filex=FALSE){
  if(is.null(trno)){
    filex_trno_tbl <- tibble(filex_name = filex_name,
                             trno = map(filex_name,~{NULL}))
  }else{
    filex_trno_tbl <- tibble(filex_name = filex_name,
                             trno = trno)
  }
  expmt_tbl <- filex_trno_tbl %>%
    group_by(filex_name) %>%
    group_map(~create_dssat_expmt(filex_name = .y$filex_name,
                                  trno = .x$trno[[1]],
                                  data_types = data_types,
                                  rewrite_filex = rewrite_filex)) %>%
    bind_rows()
  return(expmt_tbl)
}
