#' Create a table of experiments for parameter estimation
#'
#' @export
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

  if(is.null(trno)) trno <- rep(list(NULL), length(expmt))
  if(is.null(variables)) variables <- rep(list(NULL), length(expmt))

  expmt_trno_df <- data.frame(expmt_index = seq_along(expmt),
                              expmt = expmt,
                              trno = trno,
                              variables = variables)

  filex_trno_df |>
    with({
      mapply(prm_create_expmt,
             expmt = expmt,
             trno = trno,
             data_types = data_types,
             rewrite_filex = rewrite_filex)
    }) |>
    do.call(rbind, args = _)

}
