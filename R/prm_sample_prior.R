#' @export
#'
prm_sample_prior <- function(prm_df, n = 1){

  samples <- prm_df |>
    subset(!sapply(psampler, is.null)) |>
    with(psampler) |>
    lapply(\(.fun) .fun(n)) |>
    do.call(cbind, args = _)

  return(samples)
}
