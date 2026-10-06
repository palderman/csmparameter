#' Create a simulation for parameter estimation
#'
#' @export
#'
#' @param sim_args a list of arguments to be used when running the simulation
#'   with the model function
#'
#' @param data_df a data frame of seasonal summary and within-season data to use
#'  for parameter estimation. The data should be formatted in long format with
#'  columns for time (named \code{time}), variable name (named \code{variable})
#'  and observed data (named \code{obs}). Seasonal summary data should have NA
#'  for the time column.
#'
#' @param time_alias an optional additional argument providing a new name to use
#'  for the time variable. This name should match what the model returns.
#'
#' @param ... optional additional arguments providing grouping factors for
#'  identifying the simulation
#'
prm_create_sim <- function(sim_args, data_df, time_alias, ...){

  obs_df <- data_df

  if(!missing(time_alias)){
    colnames(obs_df) <-
      colnames(data_df) |>
      gsub("time", time_alias, x = _)
  }

  yhat_data_template <- obs_df |>
    subset(select = -c(obs)) |>
    list() |>
    I() |>
    data.frame(data_template = _,
               ...)

  sim_df <- data.frame(sim_args = I(list(sim_args)),
                       obs_df = I(list(obs_df)),
                       yhat_template = I(list(yhat_data_template))) |>
    prm_add_out_df() |>
    as_prm_sim_df()

  return(sim_df)

}
