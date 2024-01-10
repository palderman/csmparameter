#' @export
#'
run_optim_estimation <- function(prm_est, method='L-BFGS-B', control=list()){

  if("pmin" %in% colnames(prm_est$prm_tbl)){
    lower <- prm_est$prm_tbl$pmin
  }else{
    lower <- prm_get_pmin(prm_est$prm_tbl$pdensity)
  }
  lower <- lower[!is.na(lower)]

  if("pmax" %in% colnames(prm_est$prm_tbl)){
    upper <- prm_est$prm_tbl$pmax
  }else{
    upper <- prm_get_pmax(prm_est$prm_tbl$pdensity)
  }
  upper <- upper[!is.na(upper)]

  par_init <- prm_sample_prior(prm_est$prm_tbl, n = 1) |>
    as.vector()

  names(par_init) <- prm_est$prm_tbl$pname

  est_out <- optim(par=par_init,
                   fn = est_obj_fun,
                   prm_est = prm_est,
                   method = method,
                   lower = lower,
                   upper = upper,
                   control = control)

  return(est_out)

}
