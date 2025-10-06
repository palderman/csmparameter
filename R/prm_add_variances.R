#' Add hyperparameters for variance terms ("model error") to prm_df
#'
#' @export
#'
#' @param prm_df a data frame as created by \link{prm_create_prm_df}
#'
#' @inheritParams prm_create_prm_df
#'
prm_add_variances <- function(prm_df,
                              pmin = 0,
                              pmax = Inf,
                              pmu = 0,
                              psigma = 1,
                              pdist = "normal",
                              ...){

  dots_list <- list(...)

  var_df <- data.frame(pmin = pmin,
                       pmax = pmax,
                       pmu = pmu,
                       psigma = psigma,
                       pdist = pdist,
                       ...) |>
    unique()

  # Convert all grouping variables to character
  for(nm in names(dots_list)){
    if("POSIXt" %in% class(var_df[[nm]])){
      var_df[[nm]] <- format(var_df[[nm]], "%Y-%m-%d")
    }else if(!is.character(var_df[[nm]])){
      var_df[[nm]] <- as.character(var_df[[nm]])
    }
  }

  var_df$pname <-
    names(dots_list) |>
    lapply(\(.nm){
      paste0(.nm, ":", var_df[[.nm]])
    }) |>
    c("sigma_r", x = _, sep = ";") |>
    do.call(paste, args = _)

  output <- var_df |>
    # Create parameter table for variance parameters
    with({
      prm_create_prm_df(pname = pname,
                        pmin = pmin,
                        pmax = pmax,
                        pmu = pmu,
                        psigma = psigma,
                        pdist = pdist)
    }) |>
    within({
      pnum = pnum + max(prm_df$pnum)
    }) |>
    # Combine variance parameter table with original prm_df
    list(x = prm_df, y = _, all = TRUE) |>
    do.call(merge, args = _) |>
    arrange_df("pnum")

  return(output)
}
