prm_prior_log_density <- function(pval, prm_tbl){

  pd_index <- which(!unlist(lapply(prm_tbl$pdensity, is.null)))

  log_density <- 0

  for(i in seq_along(pval)){
    log_density <- log_density + prm_tbl$pdensity[[pd_index[i]]](pval[i])
  }

  return(log_density)
}
