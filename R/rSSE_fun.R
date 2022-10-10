#' @export
#'
rSSE_fun <- function(obs_tbl,sim_tbl){

  if(check_sim_data(sim_tbl)){
    rSSE <- obs_tbl %>%
      full_join(sim_tbl) %>%
      group_by(variable) %>%
      mutate(sq_err=(obs-sim)^2) %>%
      summarize(rSSE=sum(sq_err)/mean(obs)) %>%
      summarize(rSSE = sum(rSSE)) %>%
      pull(rSSE)
  }else{
    rSSE <- .Machine$double.xmax
  }

  return(rSSE)
}
