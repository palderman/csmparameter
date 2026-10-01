#' @export
#'
prm_obj_rSSE <- function(obs_df, yhat_df, ...){

  if(prm_check_yhat(yhat_df)){
    rSSE <- obs_df |>
      full_join(yhat_df)  |>
      group_by(variable)  |>
      mutate(sq_err=(obs-yhat)^2)  |>
      summarize(rSSE=sum(sq_err)/mean(obs))  |>
      summarize(rSSE = sum(rSSE)) |>
      pull(rSSE)
  }else{
    rSSE <- .Machine$double.xmax
  }

  return(rSSE)
}
