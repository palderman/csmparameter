#' @export
#'
#' @importFrom dplyr summarize bind_rows full_join pull
#' @importFrom tibble tibble
#' @importFrom purrr map
#' @importFrom tidyr unnest
#'
prm_create_prm_est <- function(expmt_tbl, input_tbl,
                               prm_tbl, obj_fun, dssat_call){

  if(missing(dssat_call)){
    dssat_exec <- getOption("DSSAT.CSM")
    if(is.null(dssat_exec)) stop("Please include a value for the dssat_call argument or set the executable using options(DSSAT.CSM = \"<path to executable>\")")
    version <- DSSAT:::get_dssat_version()
    file_name <- paste0("DSSBatch.V", version)
    dssat_call <- dssat_exec |>
      paste("B", file_name, sep = " ")
  }

  run_tbl <- expmt_tbl |>
    summarize(filex_trno = tibble(filex_name = filex_name,trno = trno) |>
                unnest(cols=trno) |>
                list(),
              sim_template = sim_template |>
                map(~full_join(.$data_template[[1]],.$pdate[[1]])) |>
                reduce(full_join) |>
                list(),
              out_tbl = out_tbl |>
                map(~unnest(.,cols=col_names)) |>
                reduce(full_join) |>
                list(),
              dssat_call = dssat_call,
              .groups = "keep")

  obs_tbl <- expmt_tbl |>
    pull(obs_tbl) |>
    bind_rows()

  prm_est <- list(expmt_tbl = expmt_tbl,
                 input_tbl = input_tbl,
                 prm_tbl = prm_tbl,
                 run_tbl = run_tbl,
                 obs_tbl = obs_tbl,
                 obj_fun = obj_fun)

}
