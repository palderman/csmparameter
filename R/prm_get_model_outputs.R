#' Run model and return model outputs
#'
#' @export
#'
#' @param prm_est a list object as returned by prm_create_prm_est()
#'
#' @param pvals a vector of parameter values for which to generate model outputs
#'
prm_get_model_outputs <- function(prm_est, pvals){

  run_df <- prmest$run_df

  if(prm_est$model_type == "DSSAT-CSM"){

    prm_write_inputs(prm_est$input_df, prm_est$prm_df, pvals)

    outputs <- run_df |>
      by(INDICES = run_df$group,
         \(.g){
           # Write batch file
           .g[["filex_trno"]] |>
             do.call(rbind, args = _) |>
             with({DSSAT::write_dssbatch(x = filex_name,
                                         trtno = trno)})
           # Call model
           dssat_out <- tryCatch({
             .g[["model_call"]] |>
               unique() |>
               system(intern = TRUE)},
             error = function(e){NULL})
           # Read outputs
           if(!is.null(dssat_out)){
             suppressMessages(prm_read_sim(.g))
           }else{
             .g[["sim_template"]] |>
               do.call(rbind, args = _) |>
               subset(select = c("EXPERIMENT", "TRNO")) |>
               unique() |>
               within({
                 sim = NA_real_
               })
           }
         }, SIMPLIFY = FALSE) |>
      do.call(rbind, args = _)
  }else{
    paste0("prm_get_model_outputs() not defined for model_type = \"",
           prm_est$model_type,"\"") |>
      stop()
  }

  return(outputs)
}
