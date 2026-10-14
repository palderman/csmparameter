#' Create a parameter sensitivity analysis object
#'
#' @export
#'
#' @inheritParams prm_create_prm_est sim_df prm_df obs_df run_fun smry_fun
#'
prm_create_prm_sa <- function(sim_df, prm_df, obs_df,
                              run_fun, smry_fun){

  obs_df <-
    obs_df |>
    subset(select = -obs) |>
    unique() |>
    within({obs = NA_real_})

  prm_sa <-
    prm_create_prm_est(sim_df = sim_df,
                       prm_df = prm_df,
                       obs_df = obs_df,
                       run_fun = run_fun,
                       smry_fun = smry_fun,
                       obj_fun = function(){NULL})

  prm_sa[["obj_fun"]] <- NULL

  prm_sa
}
