#'
#' @export
#'
prm_modify_vfmt <- function(.input, pname, pfmt){

  v_fmt <- attr(.input,'v_fmt')

  widths <- pfmt |>
    gregexpr("(?<=%)-*[0-9]+",
             text = _) |>
    regmatches(pfmt, m = _) |>
    gsub("-", "", x = _) |>
    as.numeric()

  v_fmt[pname] <- str_c('%',widths,'s')

  return(v_fmt)

}
