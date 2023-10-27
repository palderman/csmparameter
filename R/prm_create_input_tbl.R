#'
#' @export
#'
prm_create_input_tbl <- function(prm_tbl, search_path = ".", ...){

  file_list <- prm_tbl$pfile |>
    unique() |>

  file_list <- file_list[file_list != ""]

  file_name <- vector(mode = "character", length = length(file_list))
  file_processed <- vector(mode = "list", length = length(file_list))

  for(i in seq_along(file_list)){

    file_path <- file.path(search_path, file_list[i])

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

  }

  input_tbl <- data.frame(file_name = file_name)

  input_tbl$file_processed <- list(file_processed)

  input_tbl <- as_prm_input_tbl(input_tbl)

  return(input_tbl)

}
