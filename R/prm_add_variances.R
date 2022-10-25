#' Add hyperparameters for variance terms ("model error") to prm_tbl
#'
#' @export
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr summarize
#' @importFrom tidyr
#' @importFrom purrr map_chr
#' @importFrom stringr str_c
#' @importFrom tibble tibble
#'
#' @param obs_tbl a tibble as produced by the create_dssat_expmt()
#'  function grouped by the factors which identify the observation
#'  groups for which a variance term should be estimated
#'
prm_add_variances <- function(prm_tbl,
                              obs_tbl,
                              pmin = 0,
                              pmax = Inf,
                              pmu = 0,
                              psigma = 1,
                              pdist = "normal"){

  pname <- obs_tbl %>%
    summarize() %>%
    as.list() %>%
    # map() %>%
    do.call(~str_c(., sep = "_"), .)

  output <- tibble(pname = pname)

  return(output)
}
