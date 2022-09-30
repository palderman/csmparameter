#'
#' @importFrom dplyr "%>%" group_by summarize arrange pull
#'
#' @export
#'
get_pmax <- function(.prm_tbl){
  .prm_tbl %>%
    group_by(pnum) %>%
    summarize(pmax = sum(pmax)) %>%
    arrange(pnum) %>%
    pull(pmax)
}
#'
#' @importFrom dplyr "%>%" group_by summarize arrange pull
#'
#' @export
#'
get_pmin <- function(.prm_tbl){
  .prm_tbl %>%
    group_by(pnum) %>%
    summarize(pmin = sum(pmin)) %>%
    arrange(pnum) %>%
    pull(pmin)
}

#'
#' @importFrom dplyr "%>%" group_by summarize arrange pull
#'
#' @export
#'
get_pmu <- function(.prm_tbl){
  .prm_tbl %>%
    group_by(pnum) %>%
    summarize(pmu = sum(pmu)) %>%
    arrange(pnum) %>%
    pull(pmu)
}
