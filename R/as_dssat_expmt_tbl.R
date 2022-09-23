#' @export
#'
as_dssat_expmt_tbl <- function(tbl_in){
  UseMethod("as_dssat_expmt_tbl")
}

#' @export
#'
as_dssat_expmt_tbl.default <- function(tbl_in){
  if(class(tbl_in)[1] != 'dssat_expmt_tbl'){
    tbl_out <- tbl_in
    class(tbl_out) <- c('dssat_expmt_tbl',class(tbl_in))
  }else{
    tbl_out <- tbl_in
  }
  return(tbl_out)
}
