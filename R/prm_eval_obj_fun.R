#' Evaluate the objective function
#'
#' @export
#'
prm_eval_obj_fun <- function(pvals, prm_est){

  sim_df <- suppressMessages(prm_get_model_outputs(prm_est, pvals)) |>
    bind_rows()

  obj_fun_val <- suppressMessages(prm_est$obj_fun(prm_est$obs_df, sim_df))

  return(obj_fun_val)

}
