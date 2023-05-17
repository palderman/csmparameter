#' @export
#'
#' @importFrom DEoptim DEoptim DEoptim.control
#'
run_DEoptim_estimation <- function(prmest, control=DEoptim.control()){

  if("pmin" %in% colnames(prmest$prm_tbl)){
    lower <- prmest$prm_tbl$pmin
  }else{
    lower <- prm_get_pmin(prmest$prm_tbl$pdensity)
  }
  lower <- lower[!is.na(lower)]

  if("pmax" %in% colnames(prmest$prm_tbl)){
    upper <- prmest$prm_tbl$pmax
  }else{
    upper <- prm_get_pmax(prmest$prm_tbl$pdensity)
  }
  upper <- upper[!is.na(upper)]

  if(is.null(control$initialpop)){
    if(is.null(control$NP) | control$NP < 4){
      NP <- 10*length(lower)
    }else{
      NP <- control$NP
    }
    control$initialpop <- prm_sample_prior(prmest$prm_tbl, n = NP)
  }

  est_out <- DEoptim(fn = est_obj_fun,
                     lower = lower,
                     upper = upper,
                     control = control,
                     prmest = prmest)

  return(est_out)

}
