#'
#' @importFrom dplyr  mutate full_join
#'
#' @export
#'
join_filea_filet <- function(filea=NULL,filet=NULL){
  if(!is.null(filea)){
    filea <- filea |>
      mutate(DATE = as.POSIXct('0001001',format='%Y%j',tz='UTC'))
  }
  if(!is.null(filet) & !is.null(filea)){
    joined_data <- full_join(filea, filet)
  }else if(!is.null(filet)){
    joined_data <- filet
  }else{
    joined_data <- filea
  }
  return(joined_data)
}
