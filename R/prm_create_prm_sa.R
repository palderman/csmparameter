#' Create a parameter estimation object
#'
#' @export
#'
#' @param expmt_df a data frame of experiment definitions as created by
#'   \link{prm_create_expmt_df}
#'
#' @param input_df a data frame of input definitions as created by
#'   \link{prm_create_input_df}
#'
#' @param prm_df a data frame
#'
prm_create_prm_sa <- function(expmt_df, inp_df,
                               prm_df, sa_fun,
                               model_type = "DSSAT-CSM",
                               model_call){

  prm_sa <-
    prm_create_prm_est(expmt_df = expmt_df,
                       inp_df = inp_df,
                       prm_df = prm_df,
                       obj_fun = sa_fun,
                       model_type = model_type,
                       model_call = model_call)

  prm_sa[["sa_fun"]] <- prm_sa[["obj_fun"]]

  prm_sa$obj_fun <- NULL

}
