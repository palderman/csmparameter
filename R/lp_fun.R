#'
#' @importFrom tibble add_column
#' @importFrom dplyr filter mutate rowwise pull
#' @importFrom purrr map_dbl map_lgl
#'
lp_prior_density <- function(prm_tbl, pval){
  prm_tbl |>
    add_column(pval = pval) |>
    filter(map_lgl(pdensity, ~{!is.null(.x)})) |>
    rowwise() |>
    mutate(prior_lp = pdensity(pval)) |>
    pull(prior_lp) |>
    sum()
}

#' @export
#'
#' @importFrom tibble add_column
#' @importFrom dplyr full_join mutate
#'
lp_fun <- function(obs_tbl, sim_tbl, prm_tbl, pval){

  if(check_sim_data(sim_tbl)){

    # Calculate prior density
    prior_lp <- lp_prior_density(prm_tbl, pval)

    log_likelihood <- obs_tbl |>
      # Combine observed and simulated data
      full_join(sim_tbl) |>
      # Extract variance term from from pval vector
      mutate(obs_sigma = pval[lp_sigma_ind]) |>
      # Calculate log-likelihood assuming errors normally distributed
      with(dnorm(obs, mean = sim, sd = obs_sigma, log = TRUE)) |>
      sum()

    lp <- prior_lp + log_likelihood

  }else{

    lp <- -.Machine$double.xmax

  }

  return(lp)

}
