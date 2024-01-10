#' Calculate parameter values (regular and transformed)
#'
#' @export
#'
#' @importFrom dplyr filter pull
#' @importFrom purrr map_lgl map
#'
prm_calc_pval <- function(prm_tbl, pval_in = NULL, n = 1){

  ptransform_ind <- prm_tbl |>
    pull(ptransform) |>
    map_lgl(~{!is.null(.x)})

  if(is.null(pval_in)){
    pval_in <- prm_tbl |>
      prm_sample_prior(n = n)
  }else if(!is.matrix(pval_in)){
    pval_in <- matrix(pval_in, nrow = 1)
  }

  pval_transformed <- prm_tbl |>
    filter(ptransform_ind) |>
    pull(ptransform) |>
    map(~apply(pval_in, 1, .x)) |>
    (\(.x) do.call(cbind, .x)
     )()

  pval_out <- matrix(0.,
                     nrow = n,
                     ncol = nrow(prm_tbl))

  pval_out[, !ptransform_ind] <- pval_in
  pval_out[, ptransform_ind] <- pval_transformed

  return(pval_out)
}
