#' @export
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr filter pull
#' @importFrom purrr map_lgl map
#'
prm_sample_prior <- function(prm_tbl, n = 1){

  samples <- prm_tbl %>%
    filter(!map_lgl(psampler, is.null)) %>%
    pull(psampler) %>%
    map(function(fun) fun(n)) %>%
    do.call(cbind, .)

  return(samples)
}
