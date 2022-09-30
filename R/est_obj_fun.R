#' @export
#'
est_obj_fun <- function(pvals,prmest){

  write_inputs(prmest$input_tbl,prmest$prm_tbl,pvals)

  sim_tbl <- suppressMessages(get_model_outputs(prmest$run_tbl)) %>%
    bind_rows()

  stat <- suppressMessages(prmest$stat_fun(prmest$obs_tbl,sim_tbl))

  return(stat)

}
