#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr filter select rename mutate
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom DSSAT read_eco
#'
prm_from_eco_file <- function(file_path){

  eco <- read_eco(file_path)

  prm_tbl <- eco %>%
    filter(`ECO#` %in% c("999991", "999992", "DFAULT")) %>%
    pivot_longer(-`ECO#`) %>%
    pivot_wider(names_from = `ECO#`) %>%
    rename(pname = name,
           pmin = `999991`,
           pmax = `999992`,
           pmu = DFAULT) %>%
    mutate(psigma = (pmax-pmin)/6,
           pfile = basename(file_path)) %>%
    with(prm_create(pname = pname,
                    pfile = pfile,
                    pmin = pmin,
                    pmax = pmax,
                    pmu = pmu,
                    psigma = psigma,
                    pdist = "normal"))

  return(prm_tbl)
}
