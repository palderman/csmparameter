uniform_prior_sampler <- function(pmin, pmax){

  if(is.na(pmin) || is.null(pmin)) pmin <- -.Machine$double.xmax
  if(is.na(pmax) || is.null(pmax)) pmax <- .Machine$double.xmax

  function(n, p = runif(n)){
    exp(log(p) + log(pmax - pmin)) + pmin
  }
}

normal_prior_sampler <- function(pmin, pmax, pmu, psigma){

  if(is.na(pmin) || is.null(pmin)) pmin <- -Inf
  if(is.na(pmax) || is.null(pmax)) pmax <- Inf

  function(n, p = runif(n)){
    prob_min <- pnorm(pmin, mean = pmu, sd = psigma)
    prob_max <- pnorm(pmax, mean = pmu, sd = psigma)
    prob_pval <- exp(log(p)+log(prob_max - prob_min)) + prob_min
    qnorm(prob_pval, mean = pmu, sd = psigma)
  }
}

student_t_prior_sampler <- function(pmin, pmax, pmu, psigma, pnu){

  if(is.na(pmin) || is.null(pmin)) pmin <- -Inf
  if(is.na(pmax) || is.null(pmax)) pmax <- Inf

  function(n, p = runif(n)){
    prob_min <- pt((pmin - pmu)/psigma, df = pnu)
    prob_max <- pt((pmax - pmu)/psigma, df = pnu)
    prob_pval <- exp(log(p)+log(prob_max - prob_min)) + prob_min
    pmu + psigma*qt(prob_pval, df = pnu)
  }
}

inv_gamma_prior_sampler <- function(palpha, pbeta){
  function(n, p = runif(n)){
    1/qgamma(1 - p, shape = palpha, rate = pbeta)
  }
}

gamma_prior_sampler <- function(palpha, pbeta){
  function(n, p = runif(n)){
    qgamma(p, shape = palpha, rate = pbeta)
  }
}

beta_prior_sampler <- function(pmu, prho){
  function(n, p = runif(n)){
    shape1 <- pmu*(1-prho)/prho
    shape2 <-(1-pmu)*(1-prho)/prho
    qbeta(p, shape = palpha, rate = pbeta)
  }
}


#' @export
#'
prm_prior_sampler_function <- function(pdist, ...){

  check_pdist(pdist)

  if(pdist == "uniform"){
    prior_density <- uniform_prior_sampler(...)
  }else if(pdist == "normal"){
    prior_density <- normal_prior_sampler(...)
  }else if(pdist == "student_t"){
    prior_density <- student_t_prior_sampler(...)
  }else if(pdist == "inv_gamma"){
    prior_density <- inv_gamma_prior_sampler(...)
  }else if(pdist == "gamma"){
    prior_density <- gamma_prior_sampler(...)
  }else if(pdist == "beta"){
    prior_density <- beta_prior_sampler(...)
  }
}
