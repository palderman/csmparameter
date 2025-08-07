#' Create a parameter estimation object
#'
#' @export
#'
#' @param expmt_df a data frame of experiment definitions as created by
#'   \link{prm_create_expmt_df}
#'
#' @param inp_df a data frame of input definitions as created by
#'   \link{prm_create_inp_df}
#'
#' @param prm_df a data frame
#'
prm_create_prm_est <- function(expmt_df, inp_df,
                               prm_df, obj_fun,
                               model_type = "DSSAT-CSM",
                               model_call){

  if(missing(model_call)){
    dssat_exec <- getOption("DSSAT.CSM")
    if(is.null(dssat_exec)) stop("Please include a value for the model_call argument or set the executable using options(DSSAT.CSM = \"<path to executable>\")")
    version <- DSSAT:::get_dssat_version()
    file_name <- paste0("DSSBatch.V", version)
    model_call <- dssat_exec |>
      paste("B", file_name, sep = " ")
  }

  if("group" %in% colnames(expmt_df)){
    run_df <-
      expmt_df[["group"]] |>
      unique() |>
      data.frame(group = _)
  }else{
    run_df <-
      data.frame(group = 1)
    expmt_df[["group"]] <- 1
  }

  run_df[["filex_trno"]] <-
    by(expmt_df,
       expmt_df[["group"]],
       \(.x) list(data.frame(filex_name = .x$filex_name,
                             trno = .x$trno)),
       simplify = FALSE)

  run_df[["sim_template"]] <-
    by(expmt_df, expmt_df$group,
       \(.df){
           lapply(.df$sim_template,
                  FUN = unlist,
                  recursive = FALSE) |>
           lapply(\(.x) merge(.x$data_template,
                              .x$pdate,
                              all = TRUE)) |>
           Reduce(\(.x, .y) merge(.x, .y, all = TRUE),
                  x = _)
       }, simplify = FALSE)


  run_df[["out_df"]] <-
    by(expmt_df, expmt_df$group,
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
    expmt_df[["obs_df"]] |>
    do.call(rbind, args = _)

  prm_est <- list(expmt_df = expmt_df,
                 input_df = input_df,
                 prm_df = prm_df,
                 run_df = run_df,
                 obs_df = obs_df,
                 obj_fun = obj_fun)

}
