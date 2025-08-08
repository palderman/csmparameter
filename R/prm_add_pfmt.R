#' Add parameter format
#'
#' @export
#'
prm_add_pfmt <- function(.prm_df, input_df){

  v_fmt <-
    input_df |>
    by(~file_name,
       \(.x){
         .x[["file_processed"]] |>
           unlist(recursive = FALSE) |>
           attr("v_fmt")},
       simplify = FALSE)

  names(v_fmt) <- input_df[["file_name"]]

  .prm_df$pfmt <- NA_character_

  for(i in 1:nrow(.prm_df)){
    if(.prm_df$pfile[i] != ""){
      .prm_df$pfmt[i] <- with(.prm_df[i,],
                               v_fmt[[pfile]][pname])
    }
  }

  return(.prm_df)

}
