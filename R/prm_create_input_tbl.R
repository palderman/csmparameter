#'
#' @export
#'
prm_create_input_tbl <- function(prm_tbl, search_path = ".", ...){

  file_list <- prm_tbl$pfile |>
    unique()

  file_list <- file_list[file_list != ""]

  file_name <- vector(mode = "character", length = length(file_list))
  file_processed <- vector(mode = "list", length = length(file_list))

  for(i in seq_along(file_list)){

    file_path <- file.path(search_path, file_list[i])

    file_name[i] <- basename(file_path)

    if(file.exists(file_name[i])) file_path <- file_name[i]

    if(!file.exists(file_path)){
      file_path <- list.files(
        path = dirname(options()$DSSAT.CSM),
        pattern = file_name[i],
        recursive = TRUE,
        full.names = TRUE)
      if(length(file_path) > 1){
        if(grepl("(CUL$)|(ECO$)|(SPE$)", file_name[i]) & any(grepl("Genotype", file_path))){
          file_path <- grep("Genotype", file_path, value = TRUE)
        }else{
          file_path <- file_path[1]
          warning(paste0("File path for ",file_name[i]," set to ",file_path))
        }
      }
      if(!file.exists(file_path)){
        stop(
          paste0("File ",file_name[i]," could not be found.")
          )
      }
    }

    if(grepl("\\.CUL$", file_name[i])){
      file_processed[[i]] <- read_cul(file_path, ...)
    }else if(grepl("\\.ECO$", file_name[i])){
      file_processed[[i]] <- read_eco(file_path, ...)
    }

  }

  input_tbl <- data.frame(file_name = file_name)

  input_tbl$file_processed <- file_processed

  input_tbl <- input_tbl |>
    add_input_template(prm_tbl) |>
    as_prm_input_tbl()

  return(input_tbl)

}
