#' Create a parameter data frame for estimation
#'
#' @export
#'
#' @param pname a character vector providing the name(s) of parameter(s) to be
#'  estimated
#'
#' @param pfile a character vector providing the file name(s) of the
#'  parameter(s) to be estimated
#'
#' @param pmin a numeric vector providing the lower bound(s) for parameter(s) to
#'  be estimated. The default is -Inf (i.e. unbounded).
#'
#' @param pmax a numeric vector providing the upper bound(s) for parameter(s) to
#'  be estimated. The default is Inf (i.e. unbounded)
#'
#' @param pmu a numeric vector providing the mean(s) of the prior
#'  distribution(s) for parameter(s) to be estimated
#'
#' @param psigma a numeric vector providing the square root(s) of the
#'  variance(s) of the prior distribution(s) for parameter(s) to be estimated
#'
#' @param pdist a character vector providing the prior distribution(s) for
#'  parameter(s) to be estimated (one of "uniform" of "normal")
#'
#' @param ptier an optional character vector providing the
#'
#' @param pkey an optional character vector providing the key value to use for
#'  matching to an entry within \code{pfile}
#'
prm_create_prm_df <- function(pname,
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
  if(all(is.null(pmu))) pmu = NA_real_
  if(all(is.null(psigma))) psigma = NA_real_
  if(all(is.null(pdist))) pdist = "uniform"
  if(all(is.null(ptier))) ptier = NA_character_
  if(all(is.null(pkey))) pkey = NA_character_
  if(all(is.null(plev))) plev = NA_real_
  if(all(is.null(pind))) pind = NA_real_
  if(all(is.null(pnum))) pnum = seq_along(pname)

  prm <- data.frame(pname = pname, pmin = pmin, pmax = pmax, pmu = pmu,
                    psigma = psigma, pdist = pdist, pfile = pfile,
                    ptier = ptier, pkey = pkey, plev = plev, pind = pind,
                    pnum = pnum, pfmt = pfmt) |>
    within({
      ptier = as.character(ptier)
      pkey = as.character(pkey)
      pdensity = mapply(prm_prior_density_function,
                        pmin = pmin,
                        pmax = pmax,
                        pmu = pmu,
                        psigma = psigma,
                        pdist = pdist,
                        SIMPLIFY = FALSE)
      psampler = mapply(prm_prior_sampler_function,
                        pmin = pmin,
                        pmax = pmax,
                        pmu = pmu,
                        psigma = psigma,
                        pdist = pdist,
                        SIMPLIFY = FALSE)
      ptransform = lapply(pname, \(.x) NULL)
    }) |>
    subset(select = -c(pmin, pmax, pmu, psigma, pdist)) |>
    prm_add_pregex() |>
    as_prm_df()

  return(prm)

}
