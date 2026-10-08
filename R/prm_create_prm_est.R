#' Create a parameter estimation object
#'
#' @export
#'
#' @param sim_df a data frame of simulation definitions as created by
#'   \link{prm_create_sim_df}
#'
#' @param prm_df a data frame of parameter definitions as created by
#'   \link{prm_create_prm_df}
#'
#' @param obs_df a data frame of observed data as created by
#'   \link{prm_create_obs_df}
#'
#' @param obj_fun a function defining the objective function that should be
#'  minimized during the parameter estimation as defined by
#'  \link{prm_create_obj_fun}. This function should accept as arguments at
#'  minimum two data frames: one for observations (\code{obs_df} and one for
#'  output of simulations (\code{yhat_df}). The function should return a scalar
#'  value.
#'
#'  @param smry_fun an optional function as created by
#'   \link{prm_create_smry_fun} that computes summary variables based on direct
#'   model outputs
#'
#' @param run_fun a function that takes a parameter vector as its first
#'  argument and a prm_est object as its second argument that calls the model
#'  and returns model output in the form of a data frame corresponding to
#'  \code{obs_df} with model simulated values in column \code{yhat}
#'
prm_create_prm_est <- function(sim_df, prm_df, obs_df,
                               run_fun, smry_fun, obj_fun){

  if(missing(sim_df)) stop("sim_df is a required argument.")
  if(missing(prm_df)) stop("prm_df is a required argument.")
  if(missing(obs_df)) stop("obs_df is a required argument.")
  if(missing(run_fun)) stop("run_fun is a required argument.")
  if(missing(smry_fun)) smry_fun <- NULL
  if(missing(obj_fun)) stop("obj_fun is a required argument.")

  prm_est <- list()

  yhat_template <-
    obs_df |>
    subset(select = -c(obs))

  list(sim_df = sim_df,
       prm_df = prm_df,
       obs_df = obs_df,
       yhat_template = yhat_template,
       run_fun = run_fun,
       smry_fun = smry_fun,
       obj_fun = obj_fun)

}
