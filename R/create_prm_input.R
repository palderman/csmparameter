#'
#' @export
#'
create_prm_input <- function(file_path, ...){

  if(file_path == "") stop("file_path must not be empty string: \"\"")

  file_name <- basename(file_path)

  if(file.exists(file_name)) file_path <- file_name

  if(!file.exists(file_path)){
    file_path <- list.files(
      path = dirname(options()$DSSAT.CSM),
      pattern = file_name,
      recursive = TRUE,
      full.names = TRUE)
    if(length(file_path) > 1){
      if(grepl("(CUL$)|(ECO$)|(SPE$)", file_name) & any(grepl("Genotype", file_path))){
        file_path <- grep("Genotype", file_path, value = TRUE)
      }else{
        file_path <- file_path[1]
        warning(paste0("File path for ",file_name," set to ",file_path))
      }
    }
    if(!file.exists(file_path)){
      stop(
        paste0("File ",file_name," could not be found.")
        )
    }
  }

  if(grepl("\\.CUL$", file_name)){
    file_processed <- read_cul(file_path, ...)
  }else if(grepl("\\.ECO$", file_name)){
    file_processed <- read_eco(file_path, ...)
  }

  input_tbl <- as_prm_input_tbl(
    data.frame(file_name = file_name, file_processed = list(file_processed))
    )

  return(input_tbl)

}
