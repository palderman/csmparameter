#' @export
#'
#' @importFrom dplyr  group_by group_walk
#' @importFrom stringr str_replace_all
#'
prm_write_inputs <- function(.input_df, .prm_df, pvals){

  pval_with_tprm <- prm_apply_transform(.prm_df, pvals)

  prm_replace <- generate_prm_replace(pval_with_tprm, .prm_df)

  .input_df |>
    group_by(file_name) |>
    group_walk(~{str_replace_all(.x$file_template[[1]],prm_replace) |>
        write(.y$file_name)
    })
  return(invisible())
}
