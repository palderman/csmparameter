prm_get_pvar <- function(pdensity, name){

  # convert function into text
  lapply(pdensity, environment) |>
    # subset to line in text that contanins regex
    lapply(\(.env) if(exists(name, envir = .env)) get(name, envir = .env) else NA_real_ ) |>
    unlist()

}

prm_get_pmu <- function(pdensity){
  prm_get_pvar(pdensity, "pmu")
}

prm_get_psigma <- function(pdensity){
  prm_get_pvar(pdensity, "psigma")
}

prm_get_pmin <- function(pdensity){
  prm_get_pvar(pdensity, "pmin")
}

prm_get_pmax <- function(pdensity){
  prm_get_pvar(pdensity, "pmax")
}
