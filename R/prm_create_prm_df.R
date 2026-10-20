#' Create a parameter data frame for estimation
#'
#' @export
#'
#' @param pname a character vector providing the name(s) of parameter(s) to be
#'  estimated
#'
#' @param pdist a character vector providing the prior distribution(s) for
#'  parameter(s) to be estimated
#'
#' @param pd_args a vector of lists each element of which contains a list of
#'  parameters for the prior distribution for the parameters in \code{pname}
#'
#' @param ... other arguments used to uniquely identify the parameter
#'
prm_create_prm_df <- function(pname,
                       pdist = c("uniform", "normal", "student_t",
                                 "gamma", "inv_gamma", "beta"),
                       pd_args = list(pmin = -Inf, pmax = Inf),
                       ...){

  if(any(is.null(pname) | is.na(pname))) warning("pname cannot be NULL or missing")
  pdist <- match.arg(pdist)

  prm <- data.frame(pname = pname, pd_args = I(pd_args), pdist = pdist, ...) |>
    within({
      pdensity = mapply(\(.dist, .args){
        do.call(prm_prior_density_function,
                args = c(list(pdist = .dist), .args))},
        .dist = pdist,
        .args = pd_args,
        SIMPLIFY = FALSE)
      psampler = mapply(\(.dist, .args){
        do.call(prm_prior_sampler_function,
                args = c(list(pdist = .dist), .args))},
        .dist = pdist,
        .args = pd_args,
        SIMPLIFY = FALSE)
      ptransform = lapply(pname, \(.x) NULL)
      pnum = seq_along(pname)
    }) |>
    subset(select = -c(pd_args, pdist)) |>
    prm_add_pregex() |>
    as_prm_df()

  standardize_column_order(prm)

}
