#' Create a parameter estimation object
#'
#' @export
#'
#' @param sim_df a data frame of simulation definitions as created by
#'   \link{prm_create_sim_df}
#'
#'
#' @param prm_df a data frame
#'
#'  @param call_model a function that calls
#'
#' @param input_df an optional data frame of input definitions as created by
#'   \link{prm_create_input_df} for models with external input files
#'
prm_create_prm_est <- function(sim_df, prm_df, obj_fun,
                               call_model,
                               model_type = "DSSAT-CSM",
                               input_df){

  prm_est <- list()

  if(missing(call_model)){
    dssat_exec <- getOption("DSSAT.CSM")
    if(is.null(dssat_exec)) stop("Please include a value for the model_call argument or set the executable using options(DSSAT.CSM = \"<path to executable>\")")
    version <- DSSAT:::get_dssat_version()
    file_name <- paste0("DSSBatch.V", version)
    model_call <- dssat_exec |>
      paste("B", file_name, sep = " ")
  }

  if("group" %in% colnames(sim_df)){
    run_df <-
      sim_df[["group"]] |>
      unique() |>
      data.frame(group = _)
  }else{
    run_df <-
      data.frame(group = 1)
    sim_df[["group"]] <- 1
  }

  run_df[["filex_trno"]] <-
    by(sim_df,
       sim_df[["group"]],
       \(.x) list(data.frame(filex_name = .x$filex_name,
                             trno = .x$trno)),
       simplify = FALSE)

  run_df[["yhat_template"]] <-
    by(sim_df, sim_df$group,
       \(.df){
           lapply(.df$yhat_template,
                  FUN = unlist,
                  recursive = FALSE) |>
           lapply(\(.x) merge(.x$data_template,
                              .x$pdate,
                              all = TRUE)) |>
           Reduce(\(.x, .y) merge(.x, .y, all = TRUE),
                  x = _)
       }, simplify = FALSE)


  run_df[["out_df"]] <-
    by(sim_df, sim_df$group,
       \(.df){
         .df[["out_df"]] |>
           lapply(\(.x) with(.x,
                             setNames(col_names, file_name) |>
                             stack() |>
                             rev() |>
                             setNames(names(.x)))) |>
           Reduce(\(.x, .y) merge(.x, .y, all = TRUE),
                  x = _)
       }, simplify = FALSE)

  run_df[["model_call"]] <- model_call

  obs_df <-
    sim_df[["obs_df"]] |>
    do.call(rbind, args = _)

  prm_est <- list(sim_df = sim_df,
                 input_df = input_df,
                 prm_df = prm_df,
                 run_df = run_df,
                 obs_df = obs_df,
                 obj_fun = obj_fun)

}
