#'
#' @importFrom dplyr "%>%"
#' @importFrom stringr str_extract str_remove
#'
#' @export
#'
modify_vfmt <- function(.input, pname, pfmt){

  v_fmt <- attr(.input,'v_fmt')

  widths <- pfmt %>%
    str_extract('(?<=%)-*[0-9]+') %>%
    str_remove('-') %>%
    as.numeric()

  v_fmt[pname] <- str_c('%',widths,'s')

  return(v_fmt)

}
