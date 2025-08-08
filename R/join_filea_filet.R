join_filea_filet <- function(filea=NULL,filet=NULL){
  if(!is.null(filea)){
    filea <- filea |>
      within({
        DATE = as.POSIXct('0001001', format='%Y%j', tz='UTC')
      })
  }
  if(!is.null(filet) & !is.null(filea)){
    joined_data <- merge(filea, filet, all = TRUE)
  }else if(!is.null(filet)){
    joined_data <- filet
  }else{
    joined_data <- filea
  }
  return(joined_data)
}
