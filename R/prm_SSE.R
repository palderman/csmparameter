#' @export
#'
prm_SSE <- function(obs_df, sim_df, ...){

  if(prm_check_sim(sim_df)){
    SSE <- obs_df |>
      full_join(sim_df) |>
      mutate(sq_err=(obs-sim)^2) |>
      filter(!is.na(sq_err)) |>
      summarize(sum_sq_err=sum(sq_err)) |>
      (\(.x) .x$sum_sq_err)()
  }else{
    SSE <- .Machine$double.xmax
  }

  return(SSE)
}
