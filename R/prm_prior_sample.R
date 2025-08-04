#'
#' @export
#'
prm_prior_sample <- function(prm_df, size = 1){

  smpl_list <- lapply(prm_df$psampler,
                      function(.x) if(is.null(.x)) NA_real_ else .x(size))

  if(size > 1){
    smpl <- do.call(cbind, smpl_list)
    tform_list <- vector(mode = "list", length = nrow(prm_df))
    for(i in 1:nrow(smpl)){
      tform_list[[i]] <- unlist(
        lapply(prm_df$ptransform,
               function(.x) if(is.null(.x)) NA_real_ else .x(smpl[i,]))
      )
    }
  }else{
    smpl <- unlist(smpl_list)
  }
    tform <- do.call(cbind, tform_list)
    tform <- unlist(tform_list)

  smpl[is.na(smpl)] <- tform[is.na(smpl)]

  return(smpl)
}
