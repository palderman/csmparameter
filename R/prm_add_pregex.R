#' Add regular expressions for parameters
#'
#' @importFrom dplyr  ungroup mutate
#' @importFrom stringr str_extract str_remove
#'
#' @export
#'
prm_add_pregex <- function(.prm_df){

  widths <- .prm_df$pfmt |>
    str_extract('(?<=%)-*[0-9]+') |>
    str_remove('-') |>
    as.numeric()

  if(length(widths > 0)){
    .prm_df$pregex <- generate_pregex(widths)
  }else{
    .prm_df$pregex <- NA_character_
  }

  return(.prm_df)

}
