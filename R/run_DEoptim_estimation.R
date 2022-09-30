#' @export
#'
#' @importFrom DEoptim DEoptim DEoptim.control
#'
run_DEoptim_estimation <- function(prmest, control=DEoptim.control()){

  par_init <- runif(nrow(prmest$prm_tbl),min=prmest$prm_tbl$pmin,max=prmest$prm_tbl$pmax)

  # names(par_init) <- prmest$prm_tbl$pname

  est_out <- DEoptim(fn = est_obj_fun,
                     lower = prmest$prm_tbl$pmin,
                     upper = prmest$prm_tbl$pmax,
                     control = control,
                     prmest = prmest)

  return(est_out)

}
