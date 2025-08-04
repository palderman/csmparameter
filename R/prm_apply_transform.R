#'
#' @export
#'
prm_apply_transform <- function(prm_df, pval){

  pval_with_tprm <- vector(mode = "numeric", length = nrow(prm_df))

  for(i in seq_along(pval_with_tprm)){
    if(is.null(prm_df$ptransform[[i]])){
      pval_with_tprm[i] <- NA
    }else{
      pval_with_tprm[i] <- prm_df$ptransform[[i]](pval)
    }
  }

  pval_with_tprm[is.na(pval_with_tprm)] <- pval

  return(pval_with_tprm)
}
