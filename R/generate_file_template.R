#'
#' @importFrom DSSAT write_cul write_eco
#'
#' @export
#'
generate_file_template <- function(file_name, file_processed){

  tmp_con <- textConnection('file_template', open='w', local=TRUE)

  if(str_detect(file_name,'\\.CUL$')){
    write_cul(file_processed, tmp_con)
  }else if(str_detect(file_name,'\\.ECO$')){
    write_eco(file_processed, tmp_con)
  }

  return(list(file_template))

}
