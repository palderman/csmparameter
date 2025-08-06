prm_prior_log_density <- function(pval, prm_df){

  stopifnot(is.vector(pval) & is.atomic(pval))
  stopifnot(is.data.frame(prm_df))

  pval_df <- prm_pval_df(pval, prm_df)

  with(pval_df,
       mapply(.d = pdensity,
              .v = pval,
              \(.d, .v) .d(.v))) |>
    unlist() |>
    sum()
}
