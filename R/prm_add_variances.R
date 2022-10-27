#' Add hyperparameters for variance terms ("model error") to prm_tbl
#'
#' @export
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr summarize mutate pull
#' @importFrom tidyr pivot_longer pivot_wider unite unnest
#' @importFrom purrr map_chr
#' @importFrom stringr str_c
#' @importFrom tibble tibble
#' @importFrom lubridate is.POSIXt
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
    ungroup() %>%
    mutate(across(where(is.POSIXt), ~format(., "%Y-%j")),
           across(where(~{!is.character(.)}), as.character)) %>%
    pivot_longer(everything()) %>%
    mutate(value = str_c(name, value, sep = ":")) %>%
    pivot_wider(values_fn = list) %>%
    unnest(everything()) %>%
    unite(all, everything(), sep = ";") %>%
    pull(all)

  output <- prm_create(pname = pname,
                       pfile = "",
                       pmin = pmin,
                       pmax = pmax,
                       pmu = pmu,
                       psigma = psigma,
                       pdist = pdist,
                       pnum = max(prm_tbl$pnum)+(1:length(pname)))

  return(output)
}
