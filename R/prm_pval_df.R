prm_pval_df <- function(pval, prm_df){

  stopifnot(is.vector(pval) & is.atomic(pval))
  stopifnot(is.data.frame(prm_df))

  pd_index <-
    prm_df$pdensity |>
    lapply(\(.x) !is.null(.x)) |>
    unlist() |>
    which()

  pd_index <- pd_index[order(prm_df$pnum[pd_index])]

  prm_df[pd_index, ] |>
    within({
      pval = pval
    })

}
