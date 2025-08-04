#' Create a parameter estimation object
#'
#' @export
#'
#' @importFrom dplyr summarize bind_rows full_join pull
#' @importFrom tibble tibble
#' @importFrom purrr map
#' @importFrom tidyr unnest
#'
#' @param expmt_df a data frame of experiment definitions as created by
#'   \link[csmparameter]{prm_create_expmt_df}
#' @param inp_df a data frame of input definitions as created by
#'   \link{prm_create_inp_df}
#' @param prm_df a data frame
#'
prm_create_prm_est <- function(expmt_df, inp_df,
                               prm_df, obj_fun,
                               model_type = "DSSAT-CSM",
                               dssat_call){

  if(missing(dssat_call)){
    dssat_exec <- getOption("DSSAT.CSM")
    if(is.null(dssat_exec)) stop("Please include a value for the dssat_call argument or set the executable using options(DSSAT.CSM = \"<path to executable>\")")
    version <- DSSAT:::get_dssat_version()
    file_name <- paste0("DSSBatch.V", version)
    dssat_call <- dssat_exec |>
      paste("B", file_name, sep = " ")
  }

  run_df <- expmt_df |>
    summarize(filex_trno = tibble(filex_name = filex_name,trno = trno) |>
                unnest(cols=trno) |>
                list(),
              sim_template = sim_template |>
                map(~full_join(.$data_template[[1]],.$pdate[[1]])) |>
                reduce(full_join) |>
                list(),
              out_df = out_df |>
                map(~unnest(.,cols=col_names)) |>
                reduce(full_join) |>
                list(),
              dssat_call = dssat_call,
              .groups = "keep")

  obs_df <- expmt_df |>
    pull(obs_df) |>
    bind_rows()

  prm_est <- list(expmt_df = expmt_df,
                 input_df = input_df,
                 prm_df = prm_df,
                 run_df = run_df,
                 obs_df = obs_df,
                 obj_fun = obj_fun)

}
