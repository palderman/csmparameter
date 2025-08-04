#'
#' @importFrom tibble add_column
#' @importFrom dplyr filter mutate rowwise pull
#' @importFrom purrr map_dbl map_lgl
#'
lp_prior_density <- function(prm_df, pval){
  prm_df |>
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
prm_log_prob <- function(obs_df, sim_df, prm_df, pval){

  if(check_sim_data(sim_df)){

    # Calculate prior density
    prior_lp <- lp_prior_density(prm_df, pval)

    log_likelihood <- obs_df |>
      # Combine observed and simulated data
      full_join(sim_df) |>
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
