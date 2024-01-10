#' @export
#'
SSE_fun <- function(obs_tbl, sim_tbl, ...){

  if(check_sim_data(sim_tbl)){
    SSE <- obs_tbl |>
      full_join(sim_tbl) |>
      mutate(sq_err=(obs-sim)^2) |>
      filter(!is.na(sq_err)) |>
      summarize(sum_sq_err=sum(sq_err)) |>
      (\(.x) .x$sum_sq_err)()
  }else{
    SSE <- .Machine$double.xmax
  }

  return(SSE)
}
