uniform_prior_sampler <- function(pmin, pmax){

  if(is.na(pmin) || is.null(pmin)) pmin <- -.Machine$double.xmax
  if(is.na(pmax) || is.null(pmax)) pmax <- .Machine$double.xmax

  prior_sampler_fun <- c(
    "function(n){",
    # define local constants
    # value_to_code() function defined in prm_prior_density.R
    value_to_code("pmin", pmin),
    value_to_code("pmax", pmax),
    #
    "pval <- exp(log(pnorm(rnorm(n)))+log(pmax - pmin))+pmin",
    "return(pval)",
    "}") |>
    (\(.x) parse(text =.x)
     )() |>
    eval()

  return(prior_sampler_fun)

}

normal_prior_sampler <- function(pmin, pmax, pmu, psigma){

  if(is.na(pmin) || is.null(pmin)) pmin <- -Inf
  if(is.na(pmax) || is.null(pmax)) pmax <- Inf

  prior_sampler_fun <- c(
    "function(n){",
    # define local constants
    value_to_code("pmin", pmin),
    value_to_code("pmax", pmax),
    value_to_code("pmu", pmu),
    value_to_code("psigma", psigma),
    #
    "prob_min <- pnorm(pmin, mean = pmu, sd = psigma)",
    "prob_max <- pnorm(pmax, mean = pmu, sd = psigma)",
    "prob_pval <- exp(log(pnorm(rnorm(n)))+log(prob_max - prob_min)) + prob_min",
    "pval <- qnorm(prob_pval, mean = pmu, sd = psigma)",
    "return(pval)",
    "}") |>
    (\(.x) parse(text =.x)
     )() |>
    eval()

  return(prior_sampler_fun)

}

#' @export
#'
prm_prior_sampler_function <- function(pmin, pmax, pmu, psigma, pdist){

  check_pdist(pdist)

  if(pdist == "uniform"){
    prior_density <- uniform_prior_sampler(pmin, pmax)
  }else if(pdist == "normal"){
    prior_density <- normal_prior_sampler(pmin, pmax, pmu, psigma)
  }

}
