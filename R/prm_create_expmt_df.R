#' Create a table of experiments for parameter estimation
#'
#' @export
#'
#' @importFrom tibble tibble
#' @importFrom dplyr  group_by group_map bind_rows
#' @importFrom purrr map
#'
#' @param expmt either a vector of file names of DSSAT-formatted experiment
#'   files (File X) or a list each element of which is a list as would be
#'    returned by \link[DSSAT]{read_filex} or \link[DSSAT]{filex_template}.
#'
#' @param trno an optional list of numeric vectors of treatment numbers to be
#'  used for the parameter estimation
#'
#' @param variables an optional list of character vectors of variable names to be used
#'   for the parameter estimation
#'
prm_create_expmt_df <- function(expmt, trno = NULL, variables = NULL){

  if(is.null(trno)) trno <- lapply(1:length(expmt), ~{NULL})
  if(is.null(variables)) variables <- lapply(1:length(expmt), ~{NULL})

  expmt_trno_df <- tibble(expmt_index = 1:length(expmt),
                           expmt = expmt,
                           trno = trno,
                           variables = variables)

  expmt_df <-
    filex_trno_df |>
    group_by(expmt_index) |>
    group_map(~prm_create_expmt(expmt = .x$expmt[[1]],
                                  trno = .x$trno[[1]],
                                  data_types = .x$data_types[[1]],
                                  rewrite_filex = rewrite_filex)) |>
      bind_rows()

  return(expmt_df)
}
