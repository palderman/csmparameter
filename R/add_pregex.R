#'
#' @importFrom dplyr  ungroup mutate
#' @importFrom stringr str_extract str_remove
#'
#' @export
#'
add_pregex <- function(.prm_tbl){

  widths <- .prm_tbl$pfmt |>
    str_extract('(?<=%)-*[0-9]+') |>
    str_remove('-') |>
    as.numeric()

  .prm_tbl$pregex <- generate_pregex(widths)

  return(.prm_tbl)

}
