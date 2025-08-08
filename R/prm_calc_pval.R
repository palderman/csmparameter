#' Calculate parameter values (regular and transformed)
#'
#' @export
#'
prm_calc_pval <- function(prm_df, pval_in = NULL, n = 1){

  ptransform_ind <- prm_df[["ptransform"]] |>
    lapply(\(.x) !is.null(.x)) |>
    unlist()

  if(is.null(pval_in)){
    pval_in <- prm_df |>
      prm_sample_prior(n = n)
  }else if(!is.matrix(pval_in)){
    pval_in <- matrix(pval_in, nrow = 1)
  }

  pval_transformed <-
    prm_df |>
    subset(ptransform_ind) |>
    with(ptransform) |>
    lapply(\(.x) apply(pval_in, 1, .x)) |>
    do.call(cbind, args = _)

  pval_out <- matrix(0.,
                     nrow = n,
                     ncol = nrow(prm_df))

  pval_out[, !ptransform_ind] <- pval_in
  pval_out[, ptransform_ind] <- pval_transformed

  return(pval_out)
}
