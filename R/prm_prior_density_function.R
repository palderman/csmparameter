check_pdist <- function(pdist, pdist_options = c("uniform", "normal", "student_t", "gamma", "inv_gamma", "beta")){
  if(!pdist %in% pdist_options){
    stop(
      paste0("pdist value is not recognized: ", pdist,
         ". Should be one of: ",
         paste(pdist_options, collapse = ", "))
      )
  }else{
    return(TRUE)
  }
}

# Function for creating local constants for prior density function
value_to_code <- function(name, val) paste0(name, " <- ", sprintf("%a", val))

tnorm_dens_adj <- function(pmin, pmax, pmu, psigma){
    log(pnorm(pmax, mean = pmu, sd = psigma) - pnorm(pmin, mean = pmu, sd = psigma))
}

tstudent_t_dens_adj <- function(pmin, pmax, pmu, psigma, pnu){
  log(pt((pmax - pmu)/psigma, df = pnu) - pt((pmin - pmu)/psigma, df = pnu))
}

tnorm_test_vector <- function(pmin, pmax, pmu, psigma){

  sort(
    unique(
      c(seq(pmu - 3*psigma, pmin, length.out = 4),
        seq(pmin, pmax, length.out = 7),
        seq(pmax, 3*psigma, length.out = 4)
      )
    )
  )

}

normal_prior_density <- function(pmin, pmax, pmu, psigma){

  if(missing(pmin) || is.na(pmin) || is.null(pmin)){
    pmin <- -Inf
  }
  if(missing(pmax) || is.na(pmax) || is.null(pmax)){
    pmax <- Inf
  }

  function(pval){
    if(pval <= pmin || pval >= pmax) return(-Inf)
    dens_adj <- tnorm_dens_adj(pmin, pmax, pmu, psigma)
    dnorm(pval, mean = pmu, sd = psigma, log = TRUE) - dens_adj
  }
}

uniform_prior_density <- function(pmin, pmax){

  if(missing(pmin) || is.na(pmin) || is.null(pmin)){
    pmin <- -Inf
  }
  if(missing(pmax) || is.na(pmax) || is.null(pmax)){
    pmax <- Inf
  }

  function(pval){
    if(pval <= pmin || pval >= pmax) return(-Inf)
    dunif(pval, min = pmin, max = pmax, log = TRUE)
  }
}

student_t_prior_density <- function(pmin, pmax, pmu, psigma, pnu){

  if(missing(pmin) || is.na(pmin) || is.null(pmin)){
    pmin <- -Inf
  }
  if(missing(pmax) || is.na(pmax) || is.null(pmax)){
    pmax <- Inf
  }

    function(pval){
      if(pval <= pmin || pval >= pmax) return(-Inf)
      dens_adj <- tstudent_t_dens_adj(pmin, pmax, pnu, pmu, psigma)
      dt((pval-pmu)/psigma, df = pnu, log = TRUE) - log(psigma) - dens_adj
    }
}

inv_gamma_prior_density <- function(palpha, pbeta){
  function(pval){
    dgamma(1/pval, shape = palpha, rate = pbeta)- 2 * log(pval)
  }
}

gamma_prior_density <- function(palpha, pbeta){
  function(pval){
    dgamma(pval, shape = palpha, rate = pbeta)
  }
}

beta_prior_density <- function(pmu, prho){
  function(pval){
    shape1 <- pmu*(1-prho)/prho
    shape2 <-(1-pmu)*(1-prho)/prho
    dbeta(pval, shape = palpha, rate = pbeta)
  }
}

#' @export
#'
prm_prior_density_function <- function(pdist, ...){

  check_pdist(pdist)

  if(pdist == "uniform"){
    prior_density <- uniform_prior_density(...)
  }else if(pdist == "normal"){
    prior_density <- normal_prior_density(...)
  }else if(pdist == "student_t"){
    prior_density <- student_t_prior_density(...)
  }else if(pdist == "inv_gamma"){
    prior_density <- inv_gamma_prior_density(...)
  }else if(pdist == "gamma"){
    prior_density <- gamma_prior_density(...)
  }else if(pdist == "beta"){
    prior_density <- beta_prior_density(...)
  }

  return(prior_density)
}
