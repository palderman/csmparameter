#'
#' @export
#'
prm_sample <- function(prm_tbl, size = 1){

  smpl_list <- lapply(prm_tbl$psampler,
                      function(.x) if(is.null(.x)) NA_real_ else .x(size))

  if(size > 1){
    smpl <- do.call(cbind, smpl_list)
    tform_list <- vector(mode = "list", length = nrow(prm_tbl))
    for(i in 1:nrow(smpl)){
      tform_list[[i]] <- unlist(
        lapply(prm_tbl$ptransform,
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
