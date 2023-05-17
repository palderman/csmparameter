#' @export
#'
generate_prm_replace <- function(pvals,.prm_tbl){

  # prm_vals <- .prm_tbl %>%
  #   mutate(prm_vals = pvals[pnum]*pwt) %>%
  #   pull(prm_vals)

  not_latent <- !is.na(.prm_tbl$pfmt)

  prm_replace <- sprintf(.prm_tbl$pfmt[not_latent], pvals[not_latent])

  names(prm_replace) <- .prm_tbl$pregex[not_latent]

  return(prm_replace)

}
