#' @export
#'
#' @importFrom dplyr filter pull
#' @importFrom purrr map_lgl map
#'
prm_sample_prior <- function(prm_df, n = 1){

  samples <- prm_df |>
    filter(!map_lgl(psampler, is.null)) |>
    pull(psampler) |>
    map(function(fun) fun(n)) |>
    (\(.x) do.call(cbind, .x))()

  return(samples)
}
