#' Create a table of simulations for parameter estimation
#'
#' @export
#'
#' @param sim_args a list of arguments to be used when running simulations with
#'  the model function. Each argument should be supplied as a list. To use the
#'  same argument value for all simulations, the list should be of length one.
#'  Otherwise, the length of the list should correspond to the number of
#'  simulations
#'
#' @param ... optional additional arguments providing grouping factors for
#'  identifying the simulation
#'
prm_create_sim_df <- function(sim_args, ...){

  n_rows <- lapply(sim_args, length) |>
    unlist() |>
    max()

  sim_df <-
    sim_args |>
    lapply(\(.x) if(length(.x) < n_rows) rep(.x, n_rows) else .x) |>
    c(list(SIMPLIFY = FALSE, FUN = list)) |>
    do.call(mapply, args = _) |>
    I() |>
    data.frame(sim_args = _)

  .dot_args <- list(...)

  if(length(.dot_args) > 0){
    sim_df <-
      .dot_args |>
      lapply(\(.x) if(length(.x) < n_rows) rep(.x, n_rows) else .x) |>
      I() |>
      do.call(data.frame, args = _) |>
      cbind(sim_df)
  }else{
    sim_df <-
      data.frame(sim_no = 1:n_row) |>
      cbind(sim_df)
  }

  sim_df |>
    as_prm_sim_df()

}
