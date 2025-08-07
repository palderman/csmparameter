prm_get_pvar <- function(pdensity, regex){

  # convert function into text
  lapply(pdensity, deparse) |>
    # subset to line in text that contanins regex
    lapply(\(.x) grep(regex, .x, value = TRUE)) |>
    # Remove variable name and assignment operator
    lapply(\(.x) gsub(".*<-", "", .x)) |>
    # Convert text value into numeric
    lapply(\(.x) if(length(.x) == 0) NA_real_ else as.numeric(.x)) |>
    unlist()

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
