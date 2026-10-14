#' @export
#'
prm_sample_prior <- function(prm_df, n = 1, p){

  psampler <-
    prm_df |>
    subset(!sapply(psampler, is.null)) |>
    with(psampler)

  n_prm <- length(psampler)

  if(missing(p)){
    p <- runif(n*n_prm) |>
      matrix(nrow = n)
  }

  samples <- matrix(nrow = nrow(p), ncol = ncol(p))

  for(i in seq_along(psampler)){
    samples[,i] <- psampler[[i]](n, p[,i])
  }

  return(samples)
}
