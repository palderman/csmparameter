#' @export
#'
#' @importFrom purrr map_dbl
#'
lp_fun <- function(obs_tbl, sim_tbl, prm_tbl, pval){

  # prm_tbl %>%
  #   mutate(pval = pval) %>%
  #   rowwise() %>%
  #   mutate(prior_lp = p_dens_fun(pval), pmu, psigma, pmin, pmax))

  if(check_sim_data(sim_tbl)){
    lp <- obs_tbl %>%
      full_join(sim_tbl) %>%
      mutate(sq_err=(obs-sim)^2) %>%
      summarize(rSSE=sum(sq_err)/mean(obs)) %>%
      summarize(rSSE = sum(rSSE)) %>%
      pull(rSSE)
  }else{
    lp <- .Machine$double.xmax
  }

}
