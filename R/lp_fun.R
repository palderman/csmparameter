#' @export
#'
#' @importFrom magrittr "%>%"
#' @importFrom tibble add_column
#' @importFrom dplyr filter mutate
#' @importFrom purrr map_dbl map_lgl
#'
lp_fun <- function(obs_tbl, sim_tbl, prm_tbl, pval){

  prior_lp <- prm_tbl %>%
    filter(map_lgl(pdensity, ~{!is.null(.x)})) %>%
    add_column(pval = pval) %>%
    rowwise() %>%
    mutate(prior_lp = pdensity(pval)) %>%
    pull(prior_lp) %>%
    sum()

  lp <- prior_lp

  # if(check_sim_data(sim_tbl)){
  #   lp <- obs_tbl %>%
  #     full_join(sim_tbl) %>%
  #     mutate(sq_err=(obs-sim)^2) %>%
  #     summarize(rSSE=sum(sq_err)/mean(obs)) %>%
  #     summarize(rSSE = sum(rSSE)) %>%
  #     pull(rSSE)
  # }else{
  #   lp <- .Machine$double.xmax
  # }

  return(lp)

}
