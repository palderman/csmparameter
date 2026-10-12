#' Use the optim function to perform parameter optimization
#'
#' @export
#'
#' @param prm_est a named list created using [prm_create_prm_est]
#'
#' @param method a character string passed on to \link[stats]{optim} indicating
#'  which method to use for optimization. See \link[stats]{optim} for options
#'  and further details
#'
#' @param control a named list with options for \link[stats]{optim}() from the
#'  \code{stats} package (See \link[stats]{optim} for details)
#'
prm_est_optim <- function(prm_est, method='L-BFGS-B', control=list()){

  if("pmin" %in% colnames(prm_est[["prm_df"]])){
    lower <- prm_est[["prm_df"]][["pmin"]]
  }else{
    lower <- prm_get_pmin(prm_est[["prm_df"]][["pdensity"]])
  }
  lower <- lower[!is.na(lower)]

 if("pmax" %in% colnames(prm_est[["prm_df"]])){
    upper <- prm_est[["prm_df"]][["pmax"]]
  }else{
    upper <- prm_get_pmax(prm_est[["prm_df"]][["pdensity"]])
  }
  upper <- upper[!is.na(upper)]

  par_init <- prm_sample_prior(prm_est[["prm_df"]], n = 1, p = 0.5) |>
    as.vector()

  if(! "ndeps" %in% names(control)){
    control <-
      cbind(lower, par_init, upper) |>
      abs() |>
      log10() |>
      round() |>
      apply(2, \(.x) 10**.x) |>
      apply(1, \(.x) min(.x[.x>0])/10) |>
      list() |>
      c(ndeps = _,
        control)
  }

  if(!method %in% c("L-BFGS-B", "Brent")){
    lower <- -Inf
    upper <- Inf
  }

  true_prm <-
    prm_est[["prm_df"]][["pdensity"]] |>
    lapply(\(.x) !is.null(.x)) |>
    unlist()

  names(par_init) <- prm_est[["prm_df"]][["pname"]][true_prm]

  est_out <- optim(par=par_init,
                   fn = prm_eval_obj_fun,
                   prm_est = prm_est,
                   method = method,
                   lower = lower,
                   upper = upper,
                   control = control)

  return(est_out)

}
