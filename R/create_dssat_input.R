#'
#' @importFrom tibble tibble
#' @importFrom dplyr "%>%"
#' @importFrom stringr str_detect str_subset str_c
#' @importFrom DSSAT read_cul read_eco
#'
#' @export
#'
create_dssat_input <- function(file_path, ...){

  file_name <- basename(file_path)

  if(file.exists(file_name)) file_path <- file_name

  if(!file.exists(file_path)){
    file_path <- options()$DSSAT.CSM %>%
      dirname() %>%
      list.files(path = ., pattern = file_name, recursive = TRUE, full.names = TRUE)
    if(length(file_path) > 1){
      if(str_detect(file_name,'(CUL$)|(ECO$)|(SPE$)') & any(str_detect(file_path,'Genotype'))){
        file_path <- str_subset(file_path,'Genotype')
      }else{
        file_path <- file_path[1]
        warning(str_c("File path for ",file_name," set to ",file_path))
      }
    }
    if(!file.exists(file_path)) stop(str_c("File ",file_name," could not be found."))
  }

  if(str_detect(file_name,'\\.CUL$')){
    file_processed <- read_cul(file_path, ...)
  }else if(str_detect(file_name,'\\.ECO$')){
    file_processed <- read_eco(file_path, ...)
  }

  input_tbl <- tibble(file_name = file_name, file_processed = list(file_processed)) %>%
    as_dssat_input_tbl()

  return(input_tbl)

}
