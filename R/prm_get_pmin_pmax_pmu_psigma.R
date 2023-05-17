#'
#' @importFrom magrittr "%>%"
#' @importFrom purrr map map_dbl
#' @importFrom stringr str_subset str_remove
#'
prm_get_pvar <- function(pdensity, regex){

  unlist(
    lapply(
      lapply(
        lapply(
          # convert function into text
          lapply(pdensity, deparse),
          # subset to line in text that contanins regex
          function(.x) grep(regex, .x, value = TRUE)
        ),
        # Remove variable name and assignment operator
        function(.x) gsub(".*<-", "", .x)
      ),
      # Convert text value into numeric
      function(.x) if(length(.x) == 0) NA_real_ else as.numeric(.x)
    )
  )

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
