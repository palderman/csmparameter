prm_prior_log_density <- function(pval, prm_df){

  pd_index <- which(!unlist(lapply(prm_df$pdensity, is.null)))

  log_density <- 0

  for(i in seq_along(pval)){
    log_density <- log_density + prm_df$pdensity[[pd_index[i]]](pval[i])
  }

  return(log_density)
}
