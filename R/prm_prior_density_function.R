check_pdist <- function(pdist, pdist_options = c("normal", "uniform")){
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
value_to_code <- function(name, val) paste0(name, " <- ",
                                        deparse(val, control = c("all", "hexNumeric")))

tnorm_dens_adj <- function(pmin, pmax, pmu, psigma){
    log(pnorm(pmax, mean = pmu, sd = psigma) - pnorm(pmin, mean = pmu, sd = psigma))
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
  if(is.na(pmin) || is.null(pmin)) pmin <- -Inf
  if(is.na(pmax) || is.null(pmax)) pmax <- Inf
  prior_density_fun <- c(
    "function(pval){",
    # define local constants
    value_to_code("pmin", pmin),
    value_to_code("pmax", pmax),
    value_to_code("pmu", pmu),
    value_to_code("psigma", psigma),
    #
    "if(pval <= pmin || pval >= pmax) return(-Inf)",
    "dens_adj <- tnorm_dens_adj(pmin, pmax, pmu, psigma)",
    "lp <- dnorm(pval, mean = pmu, sd = psigma, log = TRUE) - dens_adj",
    "return(lp)",
    "}") |>
    (\(.x) parse(text =.x)
     )() |>
    eval()

  return(prior_density_fun)

}

uniform_prior_density <- function(pmin, pmax){

  if(is.na(pmin) || is.null(pmin)) pmin <- -Inf
  if(is.na(pmax) || is.null(pmax)) pmax <- Inf

  prior_density_fun <- c(
    "function(pval){",
    # define local constants
    value_to_code("pmin", pmin),
    value_to_code("pmax", pmax),
    #
    "if(pval <= pmin || pval >= pmax) return(-Inf)",
    "lp <- dunif(pval, min = pmin, max = pmax, log = TRUE)",
    "return(lp)",
    "}") |>
    (\(.x) parse(text =.x)
     )() |>
    eval()

  return(prior_density_fun)

}

#' @export
#'
prm_prior_density_function <- function(pmin, pmax, pmu, psigma, pdist){

  check_pdist(pdist)

  if(pdist == "uniform"){
    prior_density <- uniform_prior_density(pmin, pmax)
  }else if(pdist == "normal"){
    prior_density <- normal_prior_density(pmin, pmax, pmu, psigma)
  }

  return(prior_density)
}
