#' @export
#'
prm_run_optim <- function(prm_est, method='L-BFGS-B', control=list()){

  if("pmin" %in% colnames(prm_est$prm_df)){
    lower <- prm_est$prm_df$pmin
  }else{
    lower <- prm_get_pmin(prm_est$prm_df$pdensity)
  }
  lower <- lower[!is.na(lower)]

  if("pmax" %in% colnames(prm_est$prm_df)){
    upper <- prm_est$prm_df$pmax
  }else{
    upper <- prm_get_pmax(prm_est$prm_df$pdensity)
  }
  upper <- upper[!is.na(upper)]

  par_init <- prm_sample_prior(prm_est$prm_df, n = 1) |>
    as.vector()

  names(par_init) <- prm_est$prm_df$pname

  est_out <- optim(par=par_init,
                   fn = prm_eval_obj_fun,
                   prm_est = prm_est,
                   method = method,
                   lower = lower,
                   upper = upper,
                   control = control)

  return(est_out)

}
