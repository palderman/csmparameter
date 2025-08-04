#' @export
#'
prm_rSSE <- function(obs_df, sim_df, ...){

  if(prm_check_sim(sim_df)){
    rSSE <- obs_df |>
      full_join(sim_df)  |>
      group_by(variable)  |>
      mutate(sq_err=(obs-sim)^2)  |>
      summarize(rSSE=sum(sq_err)/mean(obs))  |>
      summarize(rSSE = sum(rSSE)) |>
      pull(rSSE)
  }else{
    rSSE <- .Machine$double.xmax
  }

  return(rSSE)
}
