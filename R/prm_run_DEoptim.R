#' Use the DEoptim package to perform global parameter optimization
#'
#' @export
#'
#' @param prm_est a named list created using [create_prm_est]
#'
#' @param control a named list created using \link[DEoptim]{DEoptim.control}() from the
#'  DEoptim package (See \link[DEoptim]{DEoptim.control} for details)
#'
prm_run_DEoptim <- function(prm_est, control=DEoptim::DEoptim.control()){

  if(!requireNamespace("DEoptim")){
    stop("run_DEoptim_estimation() requires the DEoptim package. Please install it and try again.")
  }

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

  if(is.null(control$initialpop)){
    if(is.null(control$NP) | is.na(control$NP) | control$NP < 4){
      NP <- 10*length(lower)
    }else{
      NP <- control$NP
    }
    control$initialpop <- prm_sample_prior(prm_est$prm_df, n = NP)
  }

  est_out <- DEoptim::DEoptim(fn = prm_eval_obj_fun,
                              lower = lower,
                              upper = upper,
                              control = control,
                              prm_est = prm_est)

  return(est_out)

}
