#'
#' @importFrom dplyr group_by "%>%" group_map rowwise mutate
#'
#' @export
#'
add_pfmt <- function(.prm_tbl, input_tbl){

  v_fmt <- group_by(input_tbl,file_name) %>%
    group_map(~attr(.$file_processed[[1]],'v_fmt'))

  names(v_fmt) <- input_tbl$file_name

  .prm_tbl <- .prm_tbl %>%
    rowwise() %>%
    mutate(pfmt = v_fmt[[pfile]][pname])

  return(.prm_tbl)

}
