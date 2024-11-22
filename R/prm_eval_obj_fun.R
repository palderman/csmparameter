#' Evaluate the objective function
#'
#' @export
#'
prm_eval_obj_fun <- function(pvals, prm_est){

  prm_write_inputs(prm_est$input_tbl, prm_est$prm_tbl, pvals)

  sim_tbl <- suppressMessages(get_model_outputs(prm_est$run_tbl)) |>
    bind_rows()

  obj_fun_val <- suppressMessages(prm_est$obj_fun(prm_est$obs_tbl, sim_tbl))

  return(obj_fun_val)

}
