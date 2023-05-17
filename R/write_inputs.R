#' @export
#'
#' @importFrom dplyr "%>%" group_by group_walk
#' @importFrom stringr str_replace_all
#'
write_inputs <- function(.input_tbl, .prm_tbl, pvals){

  pval_with_tprm <- prm_apply_transform(.prm_tbl, pvals)

  prm_replace <- generate_prm_replace(pval_with_tprm, .prm_tbl)

  .input_tbl %>%
    group_by(file_name) %>%
    group_walk(~{str_replace_all(.x$file_template[[1]],prm_replace) %>%
        write(.y$file_name)
    })
  return(invisible())
}
