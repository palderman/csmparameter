#'
#' @importFrom magrittr "%>%"
#' @importFrom purrr map map_dbl
#' @importFrom stringr str_subset str_remove
#'
prm_get_pvar <- function(pdensity, regex){
  map(pdensity, deparse) %>%
    map(~str_subset(.x, regex)) %>%
    map(~str_remove(.x, ".*<-")) %>%
    map_dbl(as.numeric)
}

#' @export
#'
prm_get_pmu <- function(pdensity){
  prm_get_pvar(pdensity, "^ *pmu")
}

#' @export
#'
prm_get_psigma <- function(pdensity){
  prm_get_pvar(pdensity, "^ *psigma")
}

#' @export
#'
prm_get_pmin <- function(pdensity){
  prm_get_pvar(pdensity, "^ *pmin")
}

#' @export
#'
prm_get_pmax <- function(pdensity){
  prm_get_pvar(pdensity, "^ *pmax")
}
