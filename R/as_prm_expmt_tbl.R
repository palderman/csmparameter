#' @export
#'
as_prm_expmt_tbl <- function(tbl_in){
  UseMethod("as_prm_expmt_tbl")
}

#' @export
#'
as_prm_expmt_tbl.default <- function(tbl_in){
  if(class(tbl_in)[1] != 'prm_expmt_tbl'){
    tbl_out <- tbl_in
    class(tbl_out) <- c('prm_expmt_tbl', class(tbl_in))
  }else{
    tbl_out <- tbl_in
  }
  return(tbl_out)
}
