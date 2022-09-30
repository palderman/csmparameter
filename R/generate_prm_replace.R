#' @export
#'
#' @importFrom dplyr "%>%" mutate pull
#'
generate_prm_replace <- function(pvals,.prm_tbl){

  prm_vals <- .prm_tbl %>%
    mutate(prm_vals = pvals[pnum]*pwt) %>%
    pull(prm_vals)

  prm_replace <- .prm_tbl$pfmt %>%
    sprintf(prm_vals)

  names(prm_replace) <- .prm_tbl$pregex

  return(prm_replace)

}
