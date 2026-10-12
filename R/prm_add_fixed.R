#' Add a fixed-value parameter to a parameter estimation data frame
#'
#' @export
#'
#' @param .prm_df a data frame that contains parameter information as generated
#'  by \link{prm_create_prm_df}
#'
#' @param pname a character vector providing the name(s) of parameter(s) to be
#'  added to the parameter data frame
#'
#' @param pval a numeric vector providing the value(s) to be used for the
#'  parameter(s) in \code{pname}
#'
#' @param ... arguments passed to \link{prm_add_transform} (see
#'  \link{prm_create_prm_df} for details)
#'
prm_add_fixed <- function(.prm_df, pname, pval, ...){

  .prm_df <-
    pval |>
    format(digits = 22) |>
    paste0(pname, "~", x = _) |>
    parse(text = _) |>
    prm_add_transform(.prm_df, ptrans = _, ...)

  standardize_column_order(.prm_df)
}
