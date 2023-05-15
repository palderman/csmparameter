#' @export
as_prm_tbl <- function(tbl_in){
  UseMethod("as_prm_tbl")
}

#'
#' @importFrom tibble as_tibble
#'
#' @export
#'
as_prm_tbl.default <- function(tbl_in){

  if(class(tbl_in)[1] != 'prm_tbl'){

    tbl_out <- as_tibble(tbl_in)
    class(tbl_out) <- c('prm_tbl', class(tbl_in))

  }else{

    tbl_out <- tbl_in

  }
  return(tbl_out)
}
