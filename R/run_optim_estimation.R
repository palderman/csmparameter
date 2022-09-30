#' @export
#'
run_optim_estimation <- function(prmest,method='L-BFGS-B',control=list()){

  par_init <- runif(nrow(prmest$prm_tbl),min=prmest$prm_tbl$pmin,max=prmest$prm_tbl$pmax)

  # names(par_init) <- prmest$prm_tbl$pname

  est_out <- optim(par=par_init,
                   fn = est_obj_fun,
                   prmest = prmest,
                   method = method,
                   lower = prmest$prm_tbl$pmin,
                   upper = prmest$prm_tbl$pmax,
                   control = control)

  return(est_out)

}
