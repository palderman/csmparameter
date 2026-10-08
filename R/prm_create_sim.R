#' Create a simulation for parameter estimation
#'
#' @export
#'
#' @param sim_args a list of arguments to be used when running the simulation
#'   with the model function
#'
#' @param ... optional additional arguments providing grouping factors for
#'  identifying the simulation
#'
prm_create_sim <- function(sim_args, ...){

  sim_df <- data.frame(sim_args = I(list(sim_args)),
                       obs_df = I(list(obs_df)),
                       yhat_template = I(list(yhat_data_template))) |>
    prm_add_out_df() |>
    as_prm_sim_df()

  return(sim_df)

}
