#'
#' @importFrom dplyr filter select rename mutate
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom DSSAT read_cul
#'
prm_from_cul_file <- function(file_path){

  cul <- read_cul(file_path)

  prm_tbl <- cul |>
    filter(`VAR-NAME` %in% c("MINIMA", "MAXIMA", "DEFAULT")) |>
    select(-`VAR#`, -`EXP#`, -`ECO#`) |>
    pivot_longer(-`VAR-NAME`) |>
    pivot_wider(names_from = `VAR-NAME`) |>
    rename(pname = name,
           pmin = MINIMA,
           pmax = MAXIMA,
           pmu = DEFAULT) |>
    mutate(psigma = (pmax-pmin)/6,
           pfile = basename(file_path)) |>
    with(prm_create(pname = pname,
                    pfile = pfile,
                    pmin = pmin,
                    pmax = pmax,
                    pmu = pmu,
                    psigma = psigma,
                    pdist = "normal"))

    return(prm_tbl)
}
