#' @export
#'
prm_sample_prior <- function(prm_df, n = 1, p = runif(n)){

  samples <- prm_df |>
    subset(!sapply(psampler, is.null)) |>
    with(psampler) |>
    lapply(\(.fun) .fun(n, p)) |>
    do.call(cbind, args = _)

  return(samples)
}
