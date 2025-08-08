#' Add regular expressions for parameters
#'
#' @export
#'
prm_add_pregex <- function(.prm_df){

  widths <-
    .prm_df[["pfmt"]] |>
    gregexpr("(?<=%)-*[0-9]+", text = _) |>
    regmatches(.prm_df[["pfmt"]], m = _) |>
    gsub("-", "", x = _) |>
    as.numeric()

  if(length(widths > 0)){
    .prm_df$pregex <- generate_pregex(widths)
  }else{
    .prm_df$pregex <- NA_character_
  }

  return(.prm_df)

}
