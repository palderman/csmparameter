#' Use the BayesianTools package to perform parameter optimization
#'
#' @export
#'
#' @param prm_est a named list created using [prm_create_prm_est]
#'
#' @param sampler a character string indicating which sampling algorithm to run.
#'  See \link[BayesianTools]{runMCMC} for options and further details
#'
#' @param settings a list with settings for the sampler passed on to
#'  \link[BayesianTools]{runMCMC}() from the \code{BayesianTools} package.
#'  See \link[BayesianTools]{runMCMC} for further details.
#'
#' @param ... additional arguments passed on to \link[BayesianTools]{createBayesianSetup}() from the
#'  \code{BayesianTools} package (See \link[BayesianTools]{createBayesianSetup} for details)
#'
prm_est_mcmc <- function(prm_est, sampler="DEzs", settings = NULL, ...){

  if(!suppressMessages(requireNamespace("BayesianTools", quietly = TRUE))){
    stop("prm_est_mcmc() requires the BayesianTools package. Please install it and try again.")
  }

  log_prior_density <- function(.prm){
    prm_prior_log_density(.prm, prm_est[["prm_df"]])
  }

  prior_sampler <- function(n=1){
    prm_sample_prior(prm_est[["prm_df"]], n = n)
  }

  log_likelihood <- function(.prm){

    yhat_df <- prm_est[["run_fun"]](.prm, prm_est)

    if(prm_check_yhat(yhat_df)){

      sigma_r_df <-
        prm_pval_df(.prm, prm_df) |>
        prm_sigma_r_df()

      lp <- obs_df |>
        # Combine observed and simulated data
        merge(yhat_df, all.x = TRUE) |>
        merge(sigma_r_df, all.x = TRUE) |>
        # Calculate log-likelihood assuming errors normally distributed
        with(dnorm(obs, mean = yhat, sd = pval, log = TRUE)) |>
        sum()

      if(is.infinite(lp)) lp <- -.Machine$double.xmax

    }else{

      lp <- -.Machine$double.xmax

    }
    lp
  }

  bayes_setup <-
    BayesianTools::createBayesianSetup(likelihood = log_likelihood,
                                       prior = log_prior_density,
                                       priorSampler = prior_sampler,
                                       ...)

  est_out <- BayesianTools::runMCMC(bayesianSetup = bayes_setup,
                                    sampler = sampler,
                                    settings = settings)

  return(est_out)

}
