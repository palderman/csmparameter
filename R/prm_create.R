#'
#' @importFrom dplyr  rowwise mutate ungroup select
#' @importFrom tibble tibble
#'
#' @export
#'
prm_create <- function(pname,
                       pfile = NA,
                       pmin = -Inf,
                       pmax = Inf,
                       pmu = NA_real_,
                       psigma = NA_real_,
                       pdist = "uniform",
                       ptier = NA_character_,
                       pkey = NA_character_,
                       plev = NA_integer_,
                       pind = NA_integer_,
                       pfmt = NA_character_,
                       pnum = NULL,
                       pwt = NULL){

  if(any(is.null(pname) | is.na(pname))) warning("pname cannot be NULL or missing")
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

  prm <- data.frame(pname = pname, pmin = pmin, pmax = pmax, pmu = pmu,
                    psigma = psigma, pdist = pdist, pfile = pfile,
                    ptier = ptier, pkey = pkey, plev = plev, pind = pind,
                    pnum = pnum, pfmt = pfmt) |>
    mutate(across(c(ptier, pkey), as.character)) |>
    rowwise() |>
    mutate(pdensity = list(prm_prior_density_function(pmin = pmin,
                                                      pmax = pmax,
                                                      pmu = pmu,
                                                      psigma = psigma,
                                                      pdist = pdist)),
           psampler = list(prm_prior_sampler_function(pmin = pmin,
                                                     pmax = pmax,
                                                     pmu = pmu,
                                                     psigma = psigma,
                                                     pdist = pdist)),
           ptransform = list(NULL)) |>
    ungroup() |>
    select(-pmin, -pmax, -pmu, -psigma, -pdist) |>
    add_pregex() |>
    as_prm_tbl()

  return(prm)

}
