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

  if(length(widths > 0)){
    .prm_tbl$pregex <- generate_pregex(widths)
  }else{
    .prm_tbl$pregex <- NA_character_
  }

  return(.prm_tbl)

}
