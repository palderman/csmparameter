#' @export
#'
check_sim_data <- function(obs_sim){

  if(any(is.na(obs_sim$sim))){
    warn_out <- obs_sim %>%
      filter(is.na(sim)) %>%
      {capture.output(print(.))} %>%
      c(paste0("Missing values were present in simulated output.",
               " The objective function value will be set to ",
               .Machine$double.xmax,"."),
        "The following observations were missing:",
        .) %>%
      str_c('\n')
    warning(warn_out)
    ok <- FALSE
  }else{
    ok <- TRUE
  }

  return(ok)
}
