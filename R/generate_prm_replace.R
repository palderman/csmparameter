#' @export
#'
generate_prm_replace <- function(pvals,.prm_df){

  # prm_vals <- .prm_df |>
  #   mutate(prm_vals = pvals[pnum]*pwt) |>
  #   pull(prm_vals)

  not_latent <- !is.na(.prm_df$pfmt)

  prm_replace <- sprintf(.prm_df$pfmt[not_latent], pvals[not_latent])

  names(prm_replace) <- .prm_df$pregex[not_latent]

  return(prm_replace)

}
