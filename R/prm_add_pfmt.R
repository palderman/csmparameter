#' Add parameter format
#'
#' @importFrom dplyr group_by  group_map rowwise mutate
#'
#' @export
#'
prm_add_pfmt <- function(.prm_df, input_df){

  v_fmt <- group_by(input_df,file_name) |>
    group_map(~attr(.$file_processed[[1]],'v_fmt'))

  names(v_fmt) <- input_df$file_name

  .prm_df$pfmt <- NA_character_

  for(i in 1:nrow(.prm_df)){
    if(.prm_df$pfile[i] != ""){
      .prm_df$pfmt[i] <- with(.prm_df[i,],
                               v_fmt[[pfile]][pname])
    }
  }

  return(.prm_df)

}
