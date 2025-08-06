#' @export
#'
#' @importFrom tibble add_column
#' @importFrom dplyr full_join mutate
#'
prm_log_prob <- function(obs_df, sim_df, prm_df, pval){

  if(prm_check_sim(sim_df)){

    sigma_r_df <-
      prm_pval_df(pval, prm_df) |>
      subset(grepl("^sigma_r;", pname))

    # Calculate prior density
    prior_lp <- prm_prior_log_density(pval, prm_df)

    log_likelihood <- obs_df |>
      # Combine observed and simulated data
      merge(sim_df, all.x = TRUE) |>
      merge(sigma_r_df, all.x = TRUE) |>
      # Calculate log-likelihood assuming errors normally distributed
      with(dnorm(obs, mean = sim, sd = pval, log = TRUE)) |>
      sum()

    lp <- prior_lp + log_likelihood

  }else{

    lp <- -.Machine$double.xmax

  }

  return(lp)

}
