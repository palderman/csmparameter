#' @export
#'
#' @importFrom dplyr  is_grouped_df group_by select group_map
#' @importFrom tidyr crossing unnest
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
      group_map(~{crossing(.y,.x) |>
          select(filex_trno) |>
          unnest(filex_trno) |>
          (\(.x) DSSAT::write_dssbatch(x = .x$filex_name,
                                       trtno = .x$trno)
          )()
        dssat_out <- tryCatch({
          system(.x$dssat_call, intern = TRUE)
        },error = function(e){NULL})
        if(!is.null(dssat_out)){
          suppressMessages(prm_read_sim(.x))
        }else{
          .x |>
            select(sim_template) |>
            unnest(sim_template) |>
            group_by(EXPERIMENT,TRNO) |>
            summarize() |>
            mutate(sim = NA_real_)
        }
      }) |>
      bind_rows()
  }else{
    paste0("prm_get_model_outputs() not defined for model_type = \"",
           prm_est$model_type,"\"") |>
      stop()
  }

  return(outputs)
}
