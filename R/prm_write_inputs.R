#' @export
#'
prm_write_inputs <- function(.input_df, .prm_df, pvals){

  pval_with_tprm <-
    pvals |>
    unname() |>
    prm_apply_transform(.prm_df, pval = _)

  prm_replace <- generate_prm_replace(pval_with_tprm, .prm_df)

  with(.input_df,
       mapply(.temp = file_template,
              .file_name = file_name,
              \(.temp, .file_name){
                file_out <- .temp
                for(i in seq_along(prm_replace)){
                  file_out <- gsub(names(prm_replace)[i],
                                   prm_replace[i],
                                   file_out)
                }
                write(file_out, .file_name)
              }))

  invisible()
}
