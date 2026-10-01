#' @export
#'
prm_check_yhat <- function(obs_yhat){

  if(any(is.na(obs_yhat$yhat))){
    warn_out <- obs_yhat |>
      filter(is.na(yhat)) |>
      (\(.x) capture.output(print(.x)))() |>
      (\(.x) c(
        paste0("Missing values were present in simulated output.",
               " The objective function value will be set to ",
               .Machine$double.xmax,"."),
        "The following observations were missing:",
        .x))() |>
      str_c('\n')
    warning(warn_out)
    ok <- FALSE
  }else{
    ok <- TRUE
  }

  return(ok)
}
