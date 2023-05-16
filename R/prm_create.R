#'
#' @importFrom dplyr "%>%" rowwise mutate ungroup select
#' @importFrom tibble tibble
#'
#' @export
#'
prm_create <- function(pname, pfile,
                       pmin = -Inf,
                       pmax = Inf,
                       pmu = NA,
                       psigma = NA,
                       pdist = "uniform",
                       ptier = as.character(NA),
                       pkey = as.character(NA),
                       plev = as.integer(NA),
                       pind = as.integer(NA),
                       pnum = NULL,
                       pwt = NULL){

  if(any(is.null(pname) | is.na(pname))) warning("pname cannot be NULL or missing")
  if(any(is.null(pfile) | is.na(pfile))) warning("pfile cannot be NULL or missing")

  if(all(is.null(pmin))) pmin = -Inf
  if(all(is.null(pmax))) pmax = Inf
  if(all(is.null(pmu))) pmu = NA
  if(all(is.null(psigma))) psigma = NA
  if(all(is.null(pdist))) pdist = "uniform"
  if(all(is.null(ptier))) ptier = as.character(NA)
  if(all(is.null(pkey))) pkey = as.character(NA)
  if(all(is.null(plev))) plev = as.numeric(NA)
  if(all(is.null(pind))) pind = as.numeric(NA)
  if(all(is.null(pnum))) pnum = 1:length(pname)

  prm <- tibble(pname = pname, pmin = pmin, pmax = pmax, pmu = pmu, psigma = psigma,
                pdist = pdist, pfile = pfile, ptier = ptier, pkey = pkey, plev = plev,
                pind = pind, pnum = pnum) %>%
    mutate(across(c(ptier, pkey), as.character)) %>%
    rowwise() %>%
    mutate(pdensity = list(prm_prior_density(pmin = pmin,
                                             pmax = pmax,
                                             pmu = pmu,
                                             psigma = psigma,
                                             pdist = pdist)),
           psampler = list(prm_prior_sampler(pmin = pmin,
                                             pmax = pmax,
                                             pmu = pmu,
                                             psigma = psigma,
                                             pdist = pdist))) %>%
    ungroup() %>%
    select(-pmin, -pmax, -pmu, -psigma, -pdist) %>%
    as_prm_tbl()

  return(prm)

}
